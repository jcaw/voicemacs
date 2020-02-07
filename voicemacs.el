(require 'json)
(require 'porthole)
(require 'yasnippet)


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


(defun voicemacs--queue-idle-once (time func &rest args)
  "Queue a function to run on an idle timer, but only once.

If the function is already queued, it will be replaced (and the
replacement will use the new timing). Note it will also remove
the function from ordinary timers.

Useful if you want to delay a slow function that only needs to be
run once."
  (cancel-function-timers func)
  (apply #'run-with-idle-timer (append (list time nil func) args)))


(defun voicemacs--equal (item-1 item-2)
  "Check equality of two objects - tolerates equivalent hash maps."
  (or (equal item-1 item-2)
      ;; Check JSON forms so we can tolerate hash maps.
      ;;
      ;; TODO: Use a more robust checking system here, JSON isn't great.
      (string= (json-encode item-1) (json-encode item-2))))


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


(defun voicemacs--get-data (key)
  "Get the current value of `key' in `voicemacs--data'."
  (gethash key voicemacs--data))


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
         ;; TODO: Should this have to be the last element?
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


(defun voicemacs--sync-major-mode (&rest _)
  "Sync the current major mode."
  (let ((major-mode-key 'major-mode))
    (unless (eq major-mode (voicemacs--get-data major-mode-key))
      (voicemacs-update-data major-mode-key major-mode))))


(defvar voicemacs--major-mode-timer nil
  "Timer for syncing the major mode.")


(defun voicemacs--enable-sync-major-mode ()
  (add-hook 'after-change-major-mode-hook 'voicemacs--sync-major-mode)
  ;; This is a reasonable proxy for when we're switching the buffer.
  (add-hook 'post-command-hook 'voicemacs--sync-major-mode)
  (unless voicemacs--major-mode-timer
    ;; Run periodically just in case we miss it.
    (setq voicemacs--major-mode-timer
          (run-with-idle-timer 1 0 'voicemacs--sync-major-mode))))


(defun voicemacs--disable-sync-major-mode ()
  (remove-hook 'after-change-major-mode-hook 'voicemacs--sync-major-mode)
  (remove-hook 'post-command-hook 'voicemacs--sync-major-mode)
  (when voicemacs--major-mode-timer
    (cancel-timer voicemacs--major-mode-timer)
    (setq voicemacs--major-mode-timer nil))
  ;; Sync current state immediately.
  (voicemacs--sync-major-mode))


(defun voicemacs--snippet (template)
  "Make a voicemacs-style snippet from a yas `template'.

Returns a hash table with minimal information about the snippet,
to reduce the amount of data we have to sync."
  (let ((snippet (make-hash-table)))
    (puthash "name" (yas--template-name template) snippet)
    (puthash "key" (yas--template-key template) snippet)
    ;; TODO: Maybe allow spoken forms in snippets?
    snippet))


(defun voicemacs--snippets-from-table (table)
  "Get a list of voicemacs-style snippets from a `table'.

The structure (in JSON format) will be:

  [
    {\"name\": name, \"key\": key},
    {\"name\": name, \"key\": key},
    {\"name\": name, \"key\": key},
  ]

Each dict here represents one snippet."
  (let ((snippets '()))
    (maphash (lambda (template-key template)
               (push (voicemacs--snippet template) snippets))
             (yas--table-uuidhash table))
    snippets))


(defun voicemacs--get-snippets ()
  "Get all registered snippets in voicemacs format.

The structure (in JSON format) will be:

  {
    snippet-list-name: snippet-list,
    snippet-list-name: snippet-list,
  }

See `voicemacs--snippets-from-table' for the `snippet-list'
structure."
  (let ((snippets (make-hash-table)))
    (maphash (lambda (key table)
               (puthash key (voicemacs--snippets-from-table table) snippets))
             yas--tables)
    snippets))


(defun voicemacs--active-snippet-tables ()
  "Get a list of the names of the active snippet tables."
  (mapcar 'yas--table-name (yas--get-snippet-tables)))


(defun voicemacs--sync-snippets ()
  "Update voicemacs with all registered snippets.

New snippets will only be pushed if they've changed. This
function can be slow, so don't run it regularly."
  (let ((snippets (voicemacs--get-snippets))
        (snippets-key 'yasnippets))
    ;; Compare JSON encodings because we don't know what types to expect -
    ;; difficult to compare equality. What matters are JSON forms. The question
    ;; we are asking is, "do we need to send new JSON data to the client?"
    (unless (voicemacs--equal snippets (voicemacs--get-data snippets-key))
      (voicemacs-update-data snippets-key snippets))))


(defun voicemacs--sync-snippets-idle ()
  "Sync snippets on an idle timer.

Syncing snippets takes a long time so we generally want to do it
once, after all snippet updates have been applied and Emacs is no
longer busy. We can use an idle timer for that."
  (voicemacs--queue-idle-once 0 'voicemacs--sync-snippets))


(defun voicemacs--sync-setup ()
  (voicemacs--enable-sync-major-mode))


(defun voicemacs--sync-teardown ()
  (voicemacs--disable-sync-major-mode))


(defun voicemacs--mode-disable ()
  (voicemacs--restore-title)
  (voicemacs--sync-teardown)
  (porthole-stop-server voicemacs--server-name))


(defun voicemacs--mode-enable ()
  (porthole-start-server voicemacs--server-name)
  (voicemacs--sync-setup)
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
