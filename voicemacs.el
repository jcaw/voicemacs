(require 'json)
(require 'porthole)
(require 'yasnippet)
(require 'seq)


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


(defun voicemacs--queue-once (func &rest args)
  "Queue a function to run once, during the next pause.

Note this will remove the function from any other timers."
  (cancel-function-timers func)
  (apply #'run-with-timer (append (list 0 nil func) args)))


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
  (voicemacs--queue-once 'voicemacs--sync-title))


(defun voicemacs--update-if-changed (key value)
  "Update data under `key', iff `value' is different."
  (unless (voicemacs--equal value (voicemacs--get-data key))
    (voicemacs-update-data key value)))


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


(defun voicemacs-pull-data ()
  "Get (and reset) pending changes to the data."
  ;; TODO: Optimize this so we check whether the data is actually worth
  ;;   transferring? Compare against a shadow state?
  (let ((result (voicemacs--hash-subset voicemacs--data
                                        voicemacs--unsynced-keys)))
    (setq voicemacs--unsynced-keys '())
    (voicemacs--sync-title)
    result))


(defun voicemacs--reset-data ()
  "Reset synchronization data to vanilla values."
  (setq voicemacs--unsynced-keys '())
  (setq voicemacs--data (make-hash-table))
  (voicemacs--sync-title))


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
  (when voicemacs--old-title-format
    (setq-default frame-title-format voicemacs--old-title-format)
    (setq voicemacs--old-title-format nil)))


(defun voicemacs--hook-change-buffer (func)
  "Hook a function to fire whenever the active buffer changes.

This includes switching windows.

There's no built-in hook for this so the current implementation
is a heuristic. It has a huge false-positive rate; `func' will
fire often, even if the buffer hasn't changed. Don't hook slow
functions."
  ;; TODO: Observe buffer changes more directly, these are dodgy heuristics.
  (add-hook 'after-change-major-mode-hook func)
  ;; This is a reasonable proxy for when we're switching the buffer.
  (add-hook 'post-command-hook func)
  (run-with-idle-timer 1 0 func))


(defun voicemacs--unhook-change-buffer (func)
  "Unhook a function that was hooked with `voicemacs--hook-change-buffer'."
  (remove-hook 'after-change-major-mode-hook func)
  (remove-hook 'post-command-hook func)
  (cancel-function-timers func))


(defun voicemacs--sync-major-mode (&rest _)
  "Sync the current major mode."
  (voicemacs--update-if-changed 'major-mode major-mode))


(defvar voicemacs--major-mode-timer nil
  "Timer for syncing the major mode.")


(defun voicemacs--enable-sync-major-mode ()
  (voicemacs--hook-change-buffer 'voicemacs--sync-major-mode)
  ;; Sync current state immediately.
  (voicemacs--sync-major-mode))


(defun voicemacs--disable-sync-major-mode ()
  (voicemacs--unhook-change-buffer 'voicemacs--sync-major-mode))


(defun voicemacs--active-minor-modes ()
  "Get the currently active minor modes."
  (seq-filter (lambda (symbol)
                (and (boundp symbol)
                     (symbol-value symbol)))
              (mapcar #'car minor-mode-alist)))


(defun voicemacs--sync-minor-modes (&rest _)
  "Synchronize the active minor modes."
  (voicemacs--update-if-changed 'minor-modes (voicemacs--active-minor-modes)))


(defun voicemacs--queue-sync-minor-modes (&rest _)
  "Queue synchronization of active minor modes."
  (voicemacs--queue-once 'voicemacs--sync-minor-modes))


(defun voicemacs--defined-commands ()
  "Get a list of all defined commands."
  ;; TODO: is there a dedicated variable for commands? Don't want to compute
  ;;   unless we have to.
  (seq-filter 'commandp obarray))


(defun voicemacs--sync-commands ()
  "Sync defined commands, iff they've changed."
  (voicemacs--update-if-changed 'defined-commands (voicemacs--defined-commands)))


(defun voicemacs--queue-sync-commands (&rest _)
  ;; We don't need this to fire off quickly. Idle timer may reduce overhead a
  ;; little.
  (voicemacs--queue-idle-once 1 'voicemacs--sync-commands))


(defun voicemacs--temp-disable-command-sync (original-func &rest args)
  "Temporarily stop syncing commands after every `defun'.

When loading many function definitions, we don't want to be
pushing the command sync onto a timer after every single `defun'.
We can use this to temporarily disable it, reducing overhead."
  ;; Ignore errors so we never affect/interrupt the underlying command - it
  ;; could be important, like `require' or `load'.
  (ignore-errors
    (advice-remove 'defun 'voicemacs--queue-sync-commands))
  (let ((result (apply original-func args)))
    (ignore-errors
      (advice-add 'defun :after 'voicemacs--queue-sync-commands)
      ;; Since we suppressed it, we should manually queue once.
      (voicemacs--queue-sync-commands))
    result))


(defun voicemacs--enable-sync-commands ()
  (advice-add 'defun :after 'voicemacs--queue-sync-commands)
  ;; When we load a file, there's lots of function definitions. Don't need to
  ;; sync after every one.
  (advice-add 'require :around 'voicemacs--temp-disable-command-sync)
  (advice-add 'load :around 'voicemacs--temp-disable-command-sync)
  ;; Sync current state immediately.
  (voicemacs--queue-sync-commands))


(defun voicemacs--disable-sync-commands ()
  (advice-remove 'defun 'voicemacs--queue-sync-commands)
  (advice-remove 'require 'voicemacs--remove-command-sync-advice)
  (advice-remove 'load 'voicemacs--remove-command-sync-advice))


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


(defun voicemacs--sync-snippet-tables ()
  "Update voicemacs with the currently active snippet tables."
  (voicemacs--update-if-changed 'active-yasnippet-tables (yas--modes-to-activate)))


(defun voicemacs--sync-snippets ()
  "Update voicemacs with all registered snippets.

New snippets will only be pushed if they've changed. This
function can be slow, so don't run it regularly."
  ;; Compare JSON encodings because we don't know what types to expect -
  ;; difficult to compare equality. What matters are JSON forms. The question we
  ;; are asking is, "do we need to send new JSON data to the client?"
  (voicemacs--update-if-changed 'yasnippets (voicemacs--get-snippets)))


(defun voicemacs--queue-snippet-sync (&rest _)
  "Sync snippets on an idle timer.

Syncing snippets takes a long time so we generally want to do it
once, after all snippet updates have been applied and Emacs is no
longer busy."
  (voicemacs--queue-once 'voicemacs--sync-snippets))


(defun voicemacs--enable-sync-snippet-tables ()
  "Enable synchronization of the active yasnippet tables."
  ;; TODO: Queue this? May not be worth it, not a slow function.
  (voicemacs--hook-change-buffer 'voicemacs--sync-snippet-tables))


(defun voicemacs--disable-sync-snippet-tables ()
  "Disable synchronization of the active yasnippet tables."
  (voicemacs--unhook-change-buffer 'voicemacs--sync-snippet-tables))


(defun voicemacs--enable-sync-snippets ()
  "Enable synchronization of yasnippets."
  (add-hook 'yas-after-reload-hook 'voicemacs--queue-snippet-sync)
  ;; These seem to be the two lowest-level functions that are used to add &
  ;; remove (and update) snippets.
  (advice-add 'yas--add-template :after 'voicemacs--queue-snippet-sync)
  (advice-add 'yas--remove-template-by-uuid :after 'voicemacs--queue-snippet-sync)
  ;; Sync current state immediately
  (voicemacs--sync-snippets)
  (voicemacs--enable-sync-snippet-tables))


(defun voicemacs--disable-sync-snippets ()
  "Disable synchronization of yasnippets."
  (remove-hook 'yas-after-reload-hook 'voicemacs--queue-snippet-sync)
  (advice-remove 'yas--add-template 'voicemacs--queue-snippet-sync)
  (advice-remove 'yas--remove-template-by-uuid 'voicemacs--queue-snippet-sync)
  (voicemacs--disable-sync-snippet-tables))


(defvar voicemacs--sync-setup-hook '()
  "Hook run when voicemacs enables synchronization.

Don't hook functions to this hook directly - use
`voicemacs--add-sync'.")


(defvar voicemacs--sync-teardown-hook '()
  "Hook run when voicemacs disables synchronization.

Don't hook functions to this hook directly - use
`voicemacs--add-sync'.")


;; TODO: Improve documentation
(defun voicemacs--sync-add (setup teardown)
  "Add a new data synchronizer to `voicemacs-mode'.

`setup' is the function to run when synchronization is enabled.
`teardown' is the function to run when synchornization is
disabled."
  (add-hook 'voicemacs--sync-setup-hook setup)
  (add-hook 'voicemacs--sync-teardown-hook teardown)
  (when voicemacs-mode
    ;; Hook won't run if `voicemacs-mode' is already active.
    (setup)))


(defun voicemacs--sync-setup ()
  "Perform necessary setup to enable data synchronization."
  (voicemacs--reset-data)
  (run-hooks 'voicemacs--sync-setup-hook)
  (porthole-expose-function voicemacs--server-name 'voicemacs-pull-data))


(defun voicemacs--sync-teardown ()
  "Tear down data synchronization (reverses `voicemacs--sync-setup')."
  (run-hooks 'voicemacs--sync-teardown-hook)
  ;; Defensive; probably not necessary
  (voicemacs--reset-data))


(defun voicemacs--mode-disable ()
  "Post-enable hook for `voicemacs-mode'."
  (voicemacs--restore-title)
  (voicemacs--sync-teardown)
  (porthole-stop-server voicemacs--server-name))


(defun voicemacs--mode-enable ()
  "Post-disable hook for `voicemacs-mode'."
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


(voicemacs--sync-add 'voicemacs--enable-sync-major-mode
                     'voicemacs--disable-sync-major-mode)
(voicemacs--sync-add 'voicemacs--enable-sync-commands
                     'voicemacs--disable-sync-commands)
(with-eval-after-load 'yasnippet
  (voicemacs--sync-add 'voicemacs--enable-sync-snippets
                       'voicemacs--disable-sync-snippets))


(provide 'voicemacs)
