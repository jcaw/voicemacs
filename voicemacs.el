(require 'json)
(require 'porthole)


(defgroup voicemacs nil
  "Utilities to make Emacs easier to control by voice."
  :prefix "voicemacs-")


(defconst voicemacs--server-name "voicemacs"
  "Name of the porthole server for voicemacs.

Clients should use this server for voicemacs-related RPC. Easiest
way to do that is with the Porthole Python Client.")


(defvar voicemacs--title-data ""
  "JSON-formatted string containing data that is appended to the title.

Used to communicate information to external engines without a
direct connection.")


(defconst voicemacs--title-suffix
  '(:eval (concat " ; " voicemacs--title-data))
  "The title format used by voicemacs.")


(defvar voicemacs--old-title-format nil
  "The title format before `voicemacs-mode' was enabled.")


(defvar voicemacs--data (make-hash-table)
  "Data that should be mirrored to clients.")


(defvar voicemacs--unsynced-keys nil
  "List of keys that have not yet been synced with the client.

When a key is updated, the client needs to manually grab the new
value. This list holds these keys.")


(defun voicemacs--sync-title ()
  "Make the title data reflect the internal voicemacs data."
  (setq voicemacs--title-data
        (json-encode `(("data" . ,(if voicemacs--unsynced-keys t :json-false))))))


(defun voicemacs-update-data (key value)
  "Update data `key' to be `value', and flag it to the user."
  (puthash key value voicemacs--data)
  ;; TODO: What to do if we're pushing unnecessary data? Still flag it? Handle
  ;;   at a lower level?
  (push key voicemacs--unsynced-keys)
  ;; Emacs won't respond if it's busy with a long-running task. Wait until Emacs
  ;; can receive messages before we tell the client to ping us. This won't
  ;; prevent hangs, but it should reduce them.
  (run-with-idle-timer 0 nil 'voicemacs--sync-title))


(defun voicemacs--hash-subset (hash-table keys)
  "Get a subset of a hash table with only the keys in `keys'."
  (let ((result (make-hash-table)))
    (mapcar (lambda (key)
               (puthash key (gethash key hash-table) result))
             keys)
    result))


(defun voicemacs-grab-data ()
  "Get (and reset) pending changes to the data."
  ;; TODO: Optimize this so we check whether the data is actually worth
  ;;   transferring? Compare against a shadow state?
  (let ((result (voicemacs--hash-subset voicemacs--data
                                        voicemacs--unsynced-keys)))
    (setq voicemacs--unsynced-keys '())
    (voicemacs--sync-title)
    result))


(defun voicemacs--voicemacs-title-p ()
  "Is the title currently in voicemacs format?"
  (let ((default-title (default-value 'frame-title-format)))
    (and (listp default-title)
         (member voicemacs--title-suffix default-title)
         ;; Cast to bool
         t)))


(defun voicemacs--set-title ()
  "Set the title to the voicemacs format."
  (unless (voicemacs--voicemacs-title-p)
    (setq voicemacs--old-title-format (default-value 'frame-title-format))
    (setq-default frame-title-format (list frame-title-format
                                           voicemacs--title-suffix))
    (voicemacs--sync-title)))


(defun voicemacs--restore-title ()
  "Restore the original title format.

Note this will only revert a voicemacs-formatted title. If thet
title format has been modified since, this method will leave the
new format."
  (setq-default frame-title-format voicemacs--old-title-format)
  (setq voicemacs--old-title-format nil))


(defun voicemacs--mode-disable ()
  (voicemacs--restore-title)
  ;; TODO: Stop syncer
  (porthole-stop-server voicemacs--server-name)
  )


(defun voicemacs--mode-enable ()
  (porthole-start-server voicemacs--server-name)
  ;; TODO: Start syncer
  (voicemacs--set-title))


(define-minor-mode voicemacs-mode
  "Minor mode to communicate with voice recognition software."
  :group 'voicemacs
  :global t
  :lighter nil
  :after-hook (if voicemacs-mode
                  (voicemacs--mode-enable)
                (voicemacs--mode-disable)))


(provide 'voicemacs)
