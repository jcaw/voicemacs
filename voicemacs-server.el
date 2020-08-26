(require 'cl-lib)
(require 'json-rpc-server)


(defvar voicemacs--update-response-timeout 3
  "Time to wait (in seconds) before assuming a key update failed.")


(defconst voicemacs--server-name "voicemacs-server"
  "Name to use for the Voicemacs server process.")


(defconst voicemacs--server-buffer-name "*voicemacs-server*"
  "Name to use for the Voicemacs server's buffer.")


(defvar voicemacs--server-process nil
  "Holds the Voicemacs server process (once it's started).")


(defvar voicemacs--connected-clients '()
  "List of currently connected clients.")


(defun voicemacs--broadcast-update (key new-value)
  (declare (indent 1))
  (mapc (lambda (client)
          (voicemacs--send-update client key new-value))
        voicemacs--connected-clients))


(defun voicemacs--send-update (client key new-value)
  (declare (indent 1))
  ;; TODO 1: Need to be able to handle the client just failing here.
  (when (voicemacs--authenticated? client)
    (let ((nonce (process-get client :outgoing-nonce)))
      (voicemacs--send
       client
       (voicemacs--make-request "update"
                                nonce
                                (voicemacs--make-hash-table
                                 `(("key" . ,key)
                                   ("value" . ,new-value)))))
      (puthash (format "%s" key) nonce
               (process-get client :pending-responses))
      ;; TODO: What if we're busy when we recieve the response? Will it process
      ;;   in time? Idle timer this?
      (run-with-timer voicemacs--update-response-timeout nil
                      'voicemacs--maybe-resend-update
                      client key nonce)
      ;; TODO: Handle overflow?
      (process-put client :outgoing-nonce (+ 1 nonce)))))


(defun voicemacs--maybe-resend-update (client key sent-nonce)
  "Resend `KEY' to the client, iff we have recieved no response."
  (when (eq sent-nonce (gethash key (process-get client :pending-responses)))
    ;; Use the *current* value, not the value when we first tried to sync. We
    ;; only care about transferring the most up-to-date state.
    (voicemacs--send-update client key (voicemacs--get-data key))))


(defun voicemacs--confirm-update (client key nonce)
  "Confirm that update of `KEY' with `NONCE' was successful."
  ;; We only care if it was a confirmation of the *latest* update.
  (let ((pending (process-get client :pending-responses)))
    (when (equal nonce (gethash key pending))
      (remhash key pending))))


(defun voicemacs--client-sentinel (client message)
  (when (member (process-status client) '(closed failed exit signal))
    (setq voicemacs--connected-clients
          (remove client voicemacs--connected-clients))))


(defun voicemacs--server-filter (client message)
  ;; TODO: Timeout on half-finished message?
  (let ((pos 0)
        match-pos)
    (while (setq match-pos (string-match "\0" message pos))
      (let ((full-message (concat (process-get client :message-so-far)
                                  (substring message pos match-pos))))
        ;; Ignore double "\0"
        (unless (string-empty-p full-message)
          (voicemacs--handle-message client full-message)))
      (process-put client :message-so-far "")
      (setq pos (+ match-pos (length "\0"))))
    (process-put client :message-so-far
                 (concat (process-get client :message-so-far)
                         (substring message pos)))))


;; TODO: Option for faster parsing if we know the data is in the right format?
(defun voicemacs--handle-message (client message)
  "Handle one message "
  ;; TODO: Cover arbitrary internal errors?
  (let ((decoded (condition-case nil
                     ;; TODO: Emacs <27 parsing too. Probably just hack into
                     ;;   json-rpc-server's approach.
                     (json-parse-string message)
                   'INVALID-JSON)))
    (cond
     ((eq decoded 'INVALID-JSON)
      (voicemacs--respond-error
       client nil "invalid-message" "A mangled message was received but it could not be decoded."))
     ((not (hash-table-p decoded))
      (voicemacs--respond-error client nil "invalid-message" "Message was not a dictionary."))
     (t
      (let ((type (gethash "type" decoded 'NOT-PROVIDED))
            (nonce (gethash "nonce" decoded 'NOT-PROVIDED))
            (data (gethash "data" decoded 'NOT-PROVIDED))
            (direction (gethash "direction" decoded 'NOT-PROVIDED)))
        (cond
         ;; Validate message structure first
         ;;
         ;; nonce first, so it can be included it in other errors
         ((eq nonce 'NOT-PROVIDED) (voicemacs--respond-error client nil "invalid-message"
                                                              "No `nonce' was provided."))
         ((eq type 'NOT-PROVIDED) (voicemacs--respond-error client nonce "invalid-message"
                                                             "No `type' was provided."))
         ((eq data 'NOT-PROVIDED) (voicemacs--respond-error client nonce "invalid-message"
                                                             "No `data' was provided."))
         ((eq direction 'NOT-PROVIDED)
          (voicemacs--respond-error client nonce "invalid-message"
                                     "No `direction' was provided."))
         ((not (stringp type)) (voicemacs--respond-error client nonce "invalid-message"
                                                          "`type' must be a string."))
         ((not (hash-table-p data)) (voicemacs--respond-error client nonce "invalid-message"
                                                               "`data' must be a dictionary."))
         ((not (member direction '("request" "response")))
          (voicemacs--respond-error client nonce "invalid-message"
                                     "`direction' must be either \"request\" or \"response\""))

         ;; Basic message structure is fine, now dispatch to correct handler.
         ((and (string= direction "request") (string= type "authenticate"))
          (condition-case nil
              (voicemacs--authenticate client data nonce)
            (error
             ;; They aren't authed, so don't give them much information
             (voicemacs--respond-error client nonce "internal-error"
                                        "An error occured during authorization."))))

         ((not (ignore-errors (process-get client :authenticated)))
          (voicemacs--respond-error client nonce "not-authenticated"
                                     "Client not authorized. Please request authorization."))

         ((string= direction "request")
          (cond ((string= type "json-rpc-call") (voicemacs--json-rpc-call
                                                 client nonce data))
                (t (voicemacs--respond-error client nonce "unrecognized-type"
                                              "Did not recognize this request type."))))
         ((string= direction "response")
          ;; Right now there's only one type of outgoing message, so we assume
          ;;   every response is a response to that.
          (cond ((string= type "confirm-update")
                 ;; TODO: Don't need key for this. The nonce should be enough
                 ;;   information to work with.
                 (let ((key (gethash "key" data)))
                   (if key
                       (voicemacs--confirm-update client key nonce)
                     ;; TODO: Handle erroneous response structure?
                     (message "Erroneous response structure - no key"))))
                ((string= type "error")
                 ;; TODO: How to handle error response from the client? Give up
                 ;;   or keep retrying?
                 (message "Error received during update: %s" data))
                (t
                 ;; TODO: Handle unknown response?
                 nil)
                ))
         (t nil)))))))


(defun voicemacs--authenticate (client data nonce)
  "Attempt to authenticate client based on `AUTH-KEY'."
  ;; TODO: Version handshake?
  (voicemacs--send
   client
   ;; For now, always authenticate
   (if (gethash "key" data)
       (progn
         (process-put client :authenticated t)
         (voicemacs--make-response nonce "authentication-successful" nil))
     (voicemacs--make-error nonce "invalid-credentials"
                            "The credentials supplied were invalid.")))
  ;; TODO: Better method for initial sync?
  (maphash (lambda (key value)
             (voicemacs--send-update client key value))
           voicemacs--data))


(defun voicemacs--authenticated? (client)
  "Has `CLIENT' authenticated itself yet?"
  (process-get client :authenticated))


(cl-defun voicemacs--make-hash-table (alist)
  "Make a hash table from an alist."
  (let ((table (make-hash-table :size (length alist)
                                :test 'equal)))
    (mapc (lambda (pair)
            (puthash (car pair) (cdr pair) table))
          alist)
    table))


(defun voicemacs--respond-error (client nonce error-type message)
  "Send an error response to the client."
  (voicemacs--send client (voicemacs--make-error
                            nonce error-type message)))


(defun voicemacs--make-request (type nonce data)
  "Construct an outgoing message.

You must manually increment the nonce if you send a message
constructed with this method."
  (voicemacs--make-hash-table
   ;; TODO: Maybe call this "request"?
   `(("direction" . "request")
     ("type" . ,type)
     ("nonce" . ,nonce)
     ("data" . ,data))))


(defun voicemacs--make-response (nonce type data)
  (voicemacs--make-hash-table
   `(("direction" . "response")
     ("type" . ,type)
     ("nonce" . ,nonce)
     ("data" . ,data))))


(defun voicemacs--make-error (nonce error-type error-message)
  (voicemacs--make-response
   nonce "error" (voicemacs--make-hash-table
                  `(("error-type" . ,error-type)
                    ("error-message" . ,error-message)))))


(defun voicemacs--json-rpc-call (client nonce data)
  (voicemacs--send
   client
   (voicemacs--make-response
    nonce "json-rpc-result"
    (voicemacs--make-hash-table
     `(("json-result" .
        ;; TODO: Handle malformed data
        ,(json-rpc-server-handle (gethash "call" data)
                                 voicemacs--exposed-functions)))))))


(defun voicemacs--send (client message)
  (process-send-string
   client
   ;; Guard the start too in case of hanging messages in the pipeline.
   (concat "\0"
           ;; HACK: Leverage private method of `json-rpc-server' because it
           ;;   emulates the behaviour of `json-encode' while exploiting the
           ;;   performance increase of Emacs 27's `json-serialize' (when
           ;;   possible).
           (json-rpc-server--emulate-legacy-encode message)
           "\0")))


(defun voicemacs--server-sentinel (process message)
  ;; TODO: Is `process' the server?
  ;;
  ;; TODO: Gross, do something more robust (check process status?)
  (when (string= message "deleted\n")
    (setq voicemacs--connected-clients nil)
    (setq voicemacs--server-process nil)))


(defun voicemacs--server-new-client (server client _message)
  "Handle a new connection from `CLIENT'.

Only one client can be connected at a time. If no clients are
connected, this client's connection will be accepted. If a client
is already connected, the new client will be rejected."
  ;; Connected clients must be authenticated before they can make requests.
  (process-put client :authenticated nil)
  ;; The filter might not be called with a full message. This is used to store
  ;; the part of the message that has been transferred between calls to the
  ;; filter.
  (process-put client :message-so-far "")
  (process-put client :outgoing-nonce 1)
  (process-put client :pending-responses (make-hash-table :test 'equal))
  (set-process-sentinel client 'voicemacs--client-sentinel)
  (push client voicemacs--connected-clients))


(defun voicemacs--start-server (&optional port plist)
  "Start a new Voicemacs server process."
  (when voicemacs--server-process
    ;; TODO: Ensure server is also running?
    (error "The Voicemacs server is already running."))
  
  (setq voicemacs--connected-clients nil)
  (setq voicemacs--server-process
        ;; TODO: Include port number in server/buffer name?
        (make-network-process :name voicemacs--server-name
                              :buffer voicemacs--server-buffer-name
                              :family 'ipv4
                              ;; TODO: Remove placeholder port
                              :service (or port 5001)
                              :sentinel 'voicemacs--server-sentinel
                              :filter 'voicemacs--server-filter
                              :log 'voicemacs--server-new-client
                              ;; :keepalive t
                              :server t
                              :noquery t
                              :plist plist))
  ;; TODO: Publish port & auth key
  )


(defun voicemacs--stop-server ()
  "Stop the Voicemacs server."
  (when voicemacs--server-process
    (delete-process voicemacs--server-process)
    (setq voicemacs--server-process nil)
    (setq voicemacs--connected-clients nil)))


(provide 'voicemacs-server)
;;; voicemacs-server.el ends here
