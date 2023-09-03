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


(defvar voicemacs--current-auth-key nil
  "Auth key clients must provide when they connect.")


(defvar voicemacs--current-port nil
  "Port that the server is running on.")


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
        (cond
         ;; Ignore double "\0" messages
         ((string-empty-p full-message) nil)
         ;; Ignore pings
         ((string= full-message "\1") nil)
         ;; Otherwise handle
         (t (voicemacs--handle-message client full-message))))
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
   (if (and voicemacs--current-auth-key  ; Protect against an error in the key.
            (equal (gethash "key" data)
                   voicemacs--current-auth-key))
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
  (when unread-command-events
    ;; HACK: Flush the command queue first - don't want RPC calls to be
    ;;   processed before queued commands.
    (message "Unread command events!")
    (redisplay))
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
  ;; TODO: Temporarily raise garbage collection threshold to stop GCs during
  ;;   this process?
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


(defun voicemacs--get-linux-temp-dir ()
  "Get a user-only temp dir on Linux, to store the server info in."
  ;; If the runtime dir isn't available, fall back to the home dir.
  (or (getenv "XDG_RUNTIME_DIR")
      (and (getenv "HOME")
           (f-join (getenv "HOME") "tmp"))
      (and (display-warning
            "voicemacs"
            (concat "Neither $XDG_RUNTIME_DIR nor $HOME could be read from "
                    "the environment. Cannot create session file - clients "
                    "will not be able to connect automatically."))
           nil)))


(defconst voicemacs--base-temp-dir
  ;; TODO: Switch this over to use the same server file style as emacs-server?
  (pcase system-type
    ('gnu/linux (voicemacs--get-linux-temp-dir))
    ('windows-nt (getenv "TEMP"))
    ('darwin (substitute-in-file-name "$HOME/Library/"))
    (_
     ;; Use the same method as Linux on unknown systems.
     (display-warning
      "voicemacs"
      (concat "Unrecognised system type. Don't know where to find the "
              "temp directory. Using the same method as Linux."))
     (porthole--get-linux-temp-dir)))
  "The base temp directory to use.

This will be dependent on the current system.")


(defconst voicemacs--session-file-name
  (f-join voicemacs--base-temp-dir "voicemacs" "session.json"))


(defun voicemacs--publish-session-file (port auth-key)
  "Publish the current session's connection information.

Clients can use this file to automatically connect."
  ;; Clean up old session first
  (when (f-file? voicemacs--session-file-name)
    (f-delete voicemacs--session-file-name))

  (let ((directory (f-dirname voicemacs--session-file-name)))
    (unless (f-dir? directory)
      (make-directory directory t)))
  (with-temp-file voicemacs--session-file-name
    (insert (json-rpc-server--emulate-legacy-encode
             (voicemacs--make-hash-table
              `(("port" . ,port)
                ("auth-key" . ,auth-key))))))
  voicemacs--session-file-name)


(defun voicemacs--erase-session-file ()
  "Delete the active session file (and directory).

Will not raise an error if the file doesn't exist."
  (when (f-file? voicemacs--session-file-name)
    (f-delete voicemacs--session-file-name))
  (let ((directory (f-dirname voicemacs--session-file-name)))
    (when (f-dir? directory)
      (f-delete directory t))))


(defun voicemacs--random-sha256-key ()
  "Generate a random sha256 key."
  ;; Make 400 random int strings, join them, then hash the result. That should
  ;; be suitably unique.
  ;;
  ;; TODO: Introduce some kind of external entropy? Command history?
  (let ((long-random-number
         (apply #'concat (mapcar (lambda (_)
                                   (format "%s" (random 9999999999999)))
                                 (number-sequence 0 400)))))
    (secure-hash 'sha256 long-random-number)))


(cl-defun voicemacs--start-server (&key port plist)
  "Start a new Voicemacs server process."
  (when voicemacs--server-process
    ;; TODO: Ensure server is also running?
    (error "The Voicemacs server is already running."))

  ;; TODO: Attach this to the server process?
  (setq voicemacs--connected-clients nil)
  (setq voicemacs--server-process
        ;; TODO: Include port number in server/buffer name?
        (make-network-process :name voicemacs--server-name
                              :buffer voicemacs--server-buffer-name
                              :family 'ipv4
                              ;; TODO: Explicit address?
                              :service (or port t)
                              :sentinel 'voicemacs--server-sentinel
                              :filter 'voicemacs--server-filter
                              :log 'voicemacs--server-new-client
                              ;; :keepalive t
                              :server t
                              :noquery t
                              :plist plist))

  (setq voicemacs--current-auth-key (voicemacs--random-sha256-key))
  (setq voicemacs--current-port
        ;; Have to manually extract the actual assigned port from the process.
        (process-contact voicemacs--server-process :service))

  (voicemacs--publish-session-file
   voicemacs--current-port
   voicemacs--current-auth-key))


(defun voicemacs--stop-server ()
  "Stop the Voicemacs server."
  (when voicemacs--server-process
    (voicemacs--erase-session-file)
    (ignore-errors
      (delete-process voicemacs--server-process))
    (setq voicemacs--current-auth-key nil
          voicemacs--current-port nil
          voicemacs--server-process nil
          voicemacs--connected-clients nil)))


(defun voicemacs--stop-server-safe ()
  "Stop the Voicemacs server, but suppress all errors.

Safe to hook to things like `kill-emacs-hook'."
  (ignore-errors (voicemacs--stop-server)))


;; In particular we want to clean up lingering session files.
(add-hook 'kill-emacs-hook #'voicemacs--stop-server-safe)
(unless voicemacs--server-process
  ;; If Emacs crashes, the session file will linger. Try and help this a little
  ;; - clean up lingering sessions when Voicemacs is loaded.
  (voicemacs--erase-session-file))


(provide 'voicemacs-server)
;;; voicemacs-server.el ends here
