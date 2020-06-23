(require 'json)
(require 'porthole)


(defgroup voicemacs nil
  "Utilities to make Emacs easier to control by voice."
  :prefix "voicemacs-")


(defconst voicemacs--server-name "voicemacs"
  "Name of the porthole server for voicemacs.

Clients should use this server for voicemacs-related RPC. Easiest
way to do that is with the Porthole Python Client.")


(defvar voicemacs--exposed-functions '()
  "Functions that should be exposed by voicemacs.")


(defvar voicemacs--title-data ""
  "JSON-formatted string containing data that is appended to the title.

Used to communicate information to external engines without a
direct connection.")


(defconst voicemacs--title-suffix
  '(:eval (concat
           ;; Delineate voicemacs data with a semicolon.
           "; "
           ;; This prefix doubles as an indicator that voicemacs is active.
           "voicemacs: "
           voicemacs--title-data
           ;; Tail with this to defend against something being added to the end
           ;; of the title.
           "; "))
  "The title format used by voicemacs.")


(defvar voicemacs--old-title-format nil
  "The title format before `voicemacs-mode' was enabled.")


(defvar voicemacs--data (make-hash-table)
  "Data that should be mirrored to clients.")


(defvar voicemacs--unsynced-keys nil
  "List of keys that have not yet been synced with the client.

When a key is updated, the client needs to manually grab the new
value. This list holds these keys.")


(defun voicemacs-expose-function (func)
  "Expose `func' over the RPC server when in `voicemacs-mode'."
  (unless (member func voicemacs--exposed-functions)
    (push func voicemacs--exposed-functions)
    (when (and voicemacs-mode (porthole-server-running-p voicemacs--server-name))
      (porthole-expose-function voicemacs--server-name func))))


;; TODO: `voicemacs-remove-function'


(cl-defun voicemacs--queue-once (func &key (args '()) (time 0))
  "Queue a function to run once, when Emacs is next idle.

If the function is already queued, it will be replaced (and the
replacement will use the new timing). Note it will also remove
the function from ordinary timers."
  (cancel-function-timers func)
  (apply #'run-with-idle-timer (append (list time nil func) args)))


(defun voicemacs--equal (item-1 item-2)
  "Check equality of two objects - tolerates equivalent hash maps."
  (or (equal item-1 item-2)
      ;; Check JSON forms so we can tolerate hash maps.
      ;;
      ;; TODO: Use a more robust checking system here, JSON isn't great.
      (string= (json-encode item-1) (json-encode item-2))))


(defun voicemacs--assert-no-semicolons (string)
  "Raise an error if `string' contains semicolons. Returns `string'."
  (mapcar (lambda (char)
            (when (= char ?\;)
              (error "String contains semicolon(s): \"%s\"" string)))
          string)
  string)


(defun voicemacs--sync-title ()
  "Make the title data reflect the internal voicemacs data."
  (setq voicemacs--title-data
        ;; Semicolons are used to separate the data section in the title - we
        ;; can't include them in the title's data.
        ;;
        ;; The point of raising an error here is to make the limitation very
        ;; overt. This error should never be raised in production, since it will
        ;; cause the entire sync mechanism to grind to a halt.
        (voicemacs--assert-no-semicolons
         (json-encode `(("data" . ,(if voicemacs--unsynced-keys t :json-false)))))))


(defun voicemacs-update-data (key value)
  "Update data `key' to be `value', and flag it to the user."
  (puthash key value voicemacs--data)
  ;; TODO: What to do if we're pushing unnecessary data? Still flag it? Handle
  ;;   at a lower level?
  (push key voicemacs--unsynced-keys)
  ;; Emacs might be busy for a while yet. If we tell the user data is available
  ;; now, they may not be able to pull it. Do it on a timer so it's flagged when
  ;; the user is more likely to be able to pull it.
  ;;
  ;; Would use an idle timer for this, but then the title wouldn't update in
  ;; e.g. the `helm-M-x' prompt.
  (run-with-timer 0 nil 'voicemacs--sync-title))


(defun voicemacs--update-if-changed (key value)
  "Update data under `key', iff `value' is different."
  (unless (voicemacs--equal value (voicemacs--get-data key))
    (voicemacs-update-data key value)))


(defun voicemacs--hash-subset (hash-table keys)
  "Get a subset of a hash table with only the keys in `keys'."
  (let ((subset (make-hash-table)))
    (mapc (lambda (key)
            (puthash key (gethash key hash-table) subset))
          keys)
    subset))


(defun voicemacs--get-data (key)
  "Get the current value of `key' in `voicemacs--data'."
  (gethash key voicemacs--data))


(defun voicemacs-pull-data (&optional full)
  "Get (and reset) pending changes to the data.

If `full' is t, gets all data, not just the changes."
  ;; TODO: Optimize this so we check whether the data is actually worth
  ;;   transferring? Compare against a shadow state?
  (voicemacs--first-result (if full
                              voicemacs--data
                            (voicemacs--hash-subset voicemacs--data
                                                    voicemacs--unsynced-keys))
    (setq voicemacs--unsynced-keys '())
    (voicemacs--sync-title)
    ;; FIX: When helm prompts are open, pulling doesn't update the title.
    ;; Seems to be a weird race condition - redrawing the modeling fixes it.
    ;;
    ;; To be safe, apply this fix whenever we're in the minibuffer.
    (when (eq major-mode 'minibuffer-inactive-mode)
      ;; TODO: Check whether this bug only occurs on Windows.
      (redraw-modeline))))


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

`SETUP' is the function to run when synchronization is enabled.
`TEARDOWN' is the function to run when synchornization is
disabled.

Note that if `voicemacs-mode' is active, any previous versions of
`SETUP' will not be torn down. That needs to be done manually
before calling `voicemacs--sync-add'."
  ;; Need to fire it manually if `voicemacs-mode' is already active.
  (when voicemacs-mode
    (funcall setup))
  (add-hook 'voicemacs--sync-setup-hook setup)
  (add-hook 'voicemacs--sync-teardown-hook teardown))


(defun voicemacs--sync-setup ()
  "Perform necessary setup to enable data synchronization."
  (voicemacs--reset-data)
  (run-hooks 'voicemacs--sync-setup-hook))


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
  (porthole-expose-functions voicemacs--server-name voicemacs--exposed-functions)
  (voicemacs--sync-setup)
  (voicemacs--set-title))


(defun voicemacs--format-symbol (string-spec &rest args)
  "Intern a symbol from a format string."
  (intern (apply 'format string-spec args)))


(defun voicemacs--define-sync-docstring (string-spec &rest args)
  "Create a docstring for a function generated by `voicemacs--define-sync'."
  (concat (apply 'format string-spec args)
          "\n\n"
          "This is an auto-generated function created by `voicemacs-define-sync'."))


(cl-defmacro voicemacs-define-sync (key &key
                                        (update 'NOT-PROVIDED)
                                        (enable 'NOT-PROVIDED)
                                        (disable 'NOT-PROVIDED)
                                        (defer t)
                                        (delay nil))
  "Register a new `KEY' to synchronize.

`KEY' should be an unquoted symbol, the key to synchronize.

This method automatically generates a number of functions that
handle synchronization (and hooks them):

`voicemacs--sync-<key>' - immediately evaluate a new value for
  `KEY', and synchronize it. `:UPDATE' should be a form that will
  be evaluated to get the new value.

`voicemacs--queue-sync-<key>' - queue `voicemacs--sync-<key>' to
  fire a single time, once Emacs is idle. This can be used to
  defer expensive synchronizations so they only happen once (when
  activity has finished).

`:DEFER' determines which of these is used as the default
synchronization function within the enable & disable
functions (described below). When `:DEFER' is t,
`voicemacs--queue-sync-<key>' will be used. When `:DEFER' is nil,
it's `voicemacs--sync-<key>'.

`voicemacs--enable-sync-<key>' - enable synchronization of this
  key. The `:ENABLE' form will be evaluated in this function to
  set up synchronization (e.g. when `voicemacs-mode' is enabled).
  For example, you may wish to attach the sync function to a
  hook, or add it as advice. Use the variable `sync-func' within
  this form to reference the default sync function.

`voicemacs--disable-sync-<key>' - disable synchronization. The
  `:DISABLE' form will be evaluated in this function to disable
  synchronization (e.g. when `voicemacs-mode' is disabled). It
  should reverse the effects of `voicemacs--enable-sync-<key>'.
  Use the variable `sync-func' within this form to reference the
  default sync function.

Pass `:DELAY' (in seconds) to increase the time before a queued
sync fires. `:DELAY' can only be passed when `:DEFER' is t.

If `voicemacs-mode' is already active, synchronization of this
key will be enabled immediately.

---

Usage examples:

  (voicemacs-define-sync in-comment
    :update (if (voicemacs-in-comment-p) t :json-false)
    :enable (run-with-idle-timer 0 0 sync-func)
    :disable (cancel-function-timers sync-func)
    :defer nil)

  (voicemacs-define-sync defined-commands
    :update (voicemacs--defined-commands)
    :enable (advice-add 'defun :after sync-func)
    :disable (advice-remove 'defun sync-func)
    :defer t
    :delay 1)

For more examples, see native uses within the `voicemacs'
source."
  (declare (indent 1))
  ;; Cache expansions
  `(let* ((-key (quote ,key))
          (-update-form (quote ,update))
          (-defer ,defer)
          (-enable-form (quote ,enable))
          (-disable-form (quote ,disable))
          (-delay ,delay))
     ;; Validate inputs
     (when (member 'NOT-PROVIDED
                   (list -key -update-form -enable-form -disable-form))
       (error "Must provide each of `KEY', `UPDATE', `ENABLE' & `DISABLE'"))
     (when (and -delay (not -defer))
       (error "`DEFER' must be `t' if a `DELAY' is provided."))
     (unless (string-match-p "[-a-zA-Z0-9]+" (format "%s" -key))
       ;; Restrict the key format so the client can deal with it easily.
       (error "`KEY' may only contain letters, numbers and dashes"))

     ;; Generate function names
     (let* ((immediate-sync-func (voicemacs--format-symbol
                                    "voicemacs--sync-%s" -key))
            (deferred-sync-func (voicemacs--format-symbol
                                   "voicemacs--queue-sync-%s" -key))
            (enable-sync-func (voicemacs--format-symbol
                                 "voicemacs--enable-sync-%s" -key))
            (disable-sync-func (voicemacs--format-symbol
                                  "voicemacs--disable-sync-%s" -key))
            (sync-action (if -defer deferred-sync-func immediate-sync-func)))
       ;; Tear down the previous version of the sync, if necessary
       (when (and voicemacs-mode (fboundp disable-sync-func))
         (funcall disable-sync-func))

       ;; Define function to sync immediately
       (eval `(defun ,immediate-sync-func (&rest _)
                ,(voicemacs--define-sync-docstring
                  (concat "Evaluate & synchronize the state of sync key `%s'."
                          "\n\nState will only be updated if it has changed.")
                  -key)
                (voicemacs--update-if-changed (quote ,-key) ,-update-form)))
       ;; Define function to sync only when idle
       (eval `(defun ,deferred-sync-func (&rest _)
                ,(voicemacs--define-sync-docstring
                  "Queue sync of key `%s' once idle." -key)
                (voicemacs--queue-once (quote ,immediate-sync-func)
                                       :time (or ,-delay 0))))

       ;; Define functions that set up & tear down syncing
       (eval `(defun ,enable-sync-func ()
                ,(voicemacs--define-sync-docstring
                  "Enable synchronization of `%s'" -key)
                (let ((sync-func (quote ,sync-action)))
                  ,-enable-form)
                ;; Sync current state up-front. Don't delay because if
                ;; `ENABLE-FORM' set up e.g. a repeating idle timer, a delay
                ;; could interfere.
                (,immediate-sync-func)))
       (eval `(defun ,disable-sync-func ()
                ,(voicemacs--define-sync-docstring
                  "Disable synchronization of `%s'" -key)
                ;; TODO: Reset data here? Is it reset somewhere else?
                (let ((sync-func (quote ,sync-action)))
                  ,-disable-form)))

       ;; Finally, register the generated setup & teardown functions
       (voicemacs--sync-add enable-sync-func disable-sync-func))))


(define-minor-mode voicemacs-mode
  "Minor mode to communicate with voice recognition software."
  :group 'voicemacs
  :global t
  :lighter nil
  :after-hook (if voicemacs-mode
                  (voicemacs--mode-enable)
                (voicemacs--mode-disable)))


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
  ;; Run it idly to catch events that fall through the net.
  (run-with-idle-timer 1 0 func))


(defun voicemacs--unhook-change-buffer (func)
  "Unhook a function that was hooked with `voicemacs--hook-change-buffer'."
  (remove-hook 'after-change-major-mode-hook func)
  (remove-hook 'post-command-hook func)
  (cancel-function-timers func))


(voicemacs-expose-function 'voicemacs-pull-data)


(provide 'voicemacs-base)
;;; voicemacs-base.el ends here
