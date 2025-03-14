(require 'json)
(require 'f)
(require 'voicemacs-server)


(defgroup voicemacs nil
  "Utilities to make Emacs easier to control by voice."
  :prefix "voicemacs-")


;; TODO: Use a set, not a list - more efficient
(defvar voicemacs--exposed-functions '()
  "Functions that should be exposed by voicemacs.")


(defvar voicemacs--data (make-hash-table :test 'equal)
  "Data that should be mirrored to clients.")


(defun voicemacs-expose-function (func)
  "Expose `FUNC' over the RPC server (when in `voicemacs-mode')."
  (unless (member func voicemacs--exposed-functions)
    (push func voicemacs--exposed-functions)))


(defun voicemacs-remove-function (func)
  "Remove a function exposed with `voicemacs-expose-function'."
  (setq voicemacs--exposed-functions
        (remove func voicemacs--exposed-functions)))


(cl-defun voicemacs--queue-once (func &key (args '()) (time 0))
  "Queue a function to run once, when Emacs is next idle.

If the function is already queued, it will be replaced (and the
replacement will use the new timing). Note it will also remove
the function from ordinary timers."
  (cancel-function-timers func)
  (apply #'run-with-idle-timer (append (list time nil func) args)))


(defun voicemacs--equal (item-1 item-2)
  "Check equality of two objects - tolerates equivalent hash maps.

Will raise an error if the items can't be serialized."
  (or (equal item-1 item-2)
      ;; Check JSON forms so we can tolerate hash maps.
      ;;
      ;; HACK: JSON comparison as a standin for in-depth compare function.
      ;;   Replace this at some point.
      (string= (json-rpc-server--emulate-legacy-encode item-1)
               (json-rpc-server--emulate-legacy-encode item-2))))


(defun voicemacs-update-data (key value)
  "Update data `key' to be `value', and send the new value to the client."
  (puthash key value voicemacs--data)
  ;; TODO: Fix circular imports
  (voicemacs--broadcast-update key value))


(defun voicemacs--update-if-changed (key value)
  "Update data under `key', iff `value' is different."
  (unless (condition-case nil
              (voicemacs--equal value (voicemacs--get-data key))
            (wrong-type-argument
             ;; HACK: Some strings can't be serialized - if that happens, return
             ;;   `t' to suppress updates.
             t))
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


(defun voicemacs--mode-disable ()
  "Post-enable hook for `voicemacs-mode'."
  (cancel-function-timers 'voicemacs--maybe-restart-server)

  ;; Disable syncing
  (run-hooks 'voicemacs--sync-teardown-hook)
  ;; Defensive, also free the memory
  (clrhash voicemacs--data)

  ;; Now stop the server.
  (voicemacs--stop-server)
  )


(defun voicemacs--maybe-restart-server ()
  "If the server is dead (and it shouldn't be), restart it."
  (when (and voicemacs-mode
             (not voicemacs--server-process))
    (message "Voicemacs server has been killed erroneously. Restarting it.")
    (voicemacs--start-server)))


(defun voicemacs--running-under-wsl ()
  "Is this Emacs instance running under Windows Subsystem for Linux?"
  (let ((uname (shell-command-to-string "uname -a")))
    (and (string-match-p "Linux" uname)
         (string-match-p "Microsoft" uname))))


(defun voicemacs--mode-enable ()
  "Post-disable hook for `voicemacs-mode'."
  ;; Start server
  (voicemacs--start-server)
  (message "Voicemacs server started")
  ;; HACK: Some packages kill the server erroneously, e.g. `persp-mode' when it
  ;;   restores the previous perspective. Don't try and play whack-a-mole with
  ;;   them, just poll & restart.
  (run-with-idle-timer 0.01 0 'voicemacs--maybe-restart-server)

  ;; Setup sync
  (clrhash voicemacs--data)
  (run-hooks 'voicemacs--sync-setup-hook)

  ;; TODO: Maybe put a signifier that voicemacs is active in the title?
  )


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
     (unless (and (or (symbolp -key) (stringp -key))
                  (string-match-p "[-a-zA-Z0-9]+" (format "%s" -key)))
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
         (with-demoted-errors "Error reversing previous Voicemacs sync:\n%s"
           (funcall disable-sync-func)))

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


(cl-defmacro voicemacs-define-sync-change-buffer (key &key
                                                      (update 'NOT-PROVIDED)
                                                      (defer t)
                                                      (delay nil))
  "Add a data key that synchronizes on buffer change.

Like `voicemacs-define-sync', but `:enable' and `:disable' are
handled for you - they will bind `sync-func' to a buffer change.
See `voicemacs-define-sync' for documentation on the remaining
parameters."
  (declare (indent 1))
  `(voicemacs-define-sync ,key
     :update ,update
     :enable (voicemacs--hook-change-buffer sync-func)
     :disable (voicemacs--unhook-change-buffer sync-func)
     :defer ,defer
     :delay ,delay))


(define-minor-mode voicemacs-mode
  "Minor mode to allow voice recognition software to communicate with Emacs."
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
  ;; TODO: Maybe hook buffer change? Have a look at it. Idle might be enough.
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


(defvar voicemacs--digits-so-far ""
  "The digits that have been pressed so far in the current numeric key sequence.

This is only used with `TODO: when key accumulation active'.")


(defconst voicemacs--process-digit-commands '()
  "All the digit processing command symbols.")


(defun voicemacs-bind-cumulative-number-commands (mode-prefix move-function keymap)
  "Make basic number keypresses select items in a voicemacs number selection context.

This tracks digit keypresses and creates commands to handle the
overall number entered, which are bound in `keymap'. Each
keypress, `move-function' is called on the raw number."
  (assert (symbolp move-function))
  (dotimes (n 10)  ; 0-9
    (eval
     `(let* ((n-str (format "%s" ,n))
             (command-symbol (intern (s-concat "voicemacs-" mode-prefix "-process-digit-" n-str))))
        (eval
         `(progn
            (defun ,command-symbol ()
              ,(format "Process the digit %s as a quickmove command in %s." mode-prefix n-str)
              (interactive)
              (unless (member last-command voicemacs--process-digit-commands)
                (setq voicemacs--digits-so-far ""))
              (setq voicemacs--digits-so-far (s-concat voicemacs--digits-so-far ,n-str))
              (condition-case err
                  (apply #',move-function (list (string-to-number voicemacs--digits-so-far)))
                (error
                 ;; If there's no valid target, we restart the sequence. This is a
                 ;; convenience feature so the user doesn't always need to tap a
                 ;; different key to restart the sequence.
                 (message "Error moving to target")
                 (setq voicemacs--digits-so-far "")

                 (apply #',move-function (list ,n))

                 ;; Note we only store the digit as part of the list if it succeeded. Otherwise, all subsequent movements will fail.
                 (setq voicemacs--digits-so-far ,n-str))))

            (add-to-list 'voicemacs--process-digit-commands ',command-symbol)

            (bind-key ,n-str #',command-symbol ',keymap)
            ;; TODO: Possibly also bind the prefixed number to act as the ordinary number command.
            )
         t))
     t))
  )





(provide 'voicemacs-base)
;;; voicemacs-base.el ends here
