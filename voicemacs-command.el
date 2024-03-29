;; This module allows the user to inject commands into the command loop.
;;
;; It should be used to execute named, interactive commands via RPC. Command
;; invoked this way will be treated very similarly to if they had been invoked
;; via keyboard input - this means they'll work in macros, etc.


(require 'bind-key)
(require 'voicemacs-base)


(defun voicemacs-inject-command (command-name &optional prefix-arg)
  "Execute a command as though it were executed via keyboard input.

This function uses emulation techniques to make the command
behave as much like a keyboard command as possible, down to
injection into the event loop. Using this method, the command can
be smoothly included in Macros, executed after existing commands
in the queue, etc. This also causes errors to be reported in a
similar manner to normal commands (rather than in the network
process filter).

It is important that RPC-based voice commands are
indistinguishable (from the user's perspective) from commands
injected via keyboard input. They may well be mixed by the
client.

Arguments:

`COMMAND-NAME' - The name of the command, as a string.

`PREFIX-ARG' - Optional. Prefix argument to add to the command."
  ;; TODO: Does this need to be run on a proxy with an idle timer?
  (let ((command-symbol (if (symbolp command-name)
                            command-name
                          ;; Assume it's a string, intern it.
                          (condition-case nil
                              (intern command-name)
                            (error "Could not intern \"%s\" as a symbol." command-name)))))
    (unless (commandp command-symbol)
      (error "`%s' is not a recognized interactive command." command-symbol))

    (unless (or (integerp prefix-arg)
                ;; Prefix arguments might look like '(4) or '(16)
                (and (listp prefix-arg)
                     (eq 1 (length prefix-arg))
                     (integerp (car prefix-arg)))
                (equal prefix-arg "-")
                ;; Also allow no prefix argument.
                (eq prefix-arg nil))
      (error (format
              "Prefix arguments may only be numbers, a list such as '(16) or a dash. Was: %s, type: %s"
              prefix-arg
              (type-of prefix-arg))))
    (voicemacs--inject-command command-symbol prefix-arg)
    ;; The return value doesn't really matter.
    "Command injected successfully."))


(defun voicemacs--inject-command (command &optional prefix-arg)
  "Inject a command into the event loop."
  (voicemacs--inject-event
   (voicemacs--create-command-event command prefix-arg)))


(defun voicemacs--create-command-event (command &optional prefix-arg)
  "Create a command emulation event.

This event can be injected into the event loop to run an
interactive command, as if it was called by a keypress."
  (voicemacs--create-event 'voicemacs-command-signal command prefix-arg))


(defun voicemacs--inject-event (event)
  "Inject an event into the command loop (at the chronological end)."
  ;; TODO: Check this definitely adds it to the end
  (setq unread-command-events (cons event unread-command-events)))


(defun voicemacs--create-event (signal-type &rest data)
  "Create a signal with some optional `DATA'.

Arguments:

`SIGNAL-TYPE' - a symbol. You can think of it as the type of
  event we're injecting. This signal should be bound to some kind
  of event handler by binding it as you would a keychord.

`DATA' - the data to send."
  (cons signal-type data))


(defun voicemacs--multiple-cursors-manual-injection (emulated-command)
  "Hack to trick `multiple-cursors-mode' into running an emulated command.

Without this `multiple-cursors-mode' will fall over when applying
our command to secondary cursors, because it tries to repeat
`voicemacs--command-event-handler', not the command we're
emulating. This happens because it stores the original command
during the `pre-command-hook', which is run automatically by the
signal handler.

To get around this we ignore `voicemacs--command-event-handler'
and manually make it store the command we're emulating. The
normal `post-command-hook' will then trigger that instead. Hairy
hack, but it's also precise with any hacks here to avoid
second-order effects."
  (when (and (bound-and-true-p multiple-cursors-mode)
             (functionp 'mc/make-a-note-of-the-command-being-run))
    (let ((this-original-command emulated-command))
      (mc/make-a-note-of-the-command-being-run))))


;;;###autoload
(with-eval-after-load 'multiple-cursors-core
  ;; HACK: Multiple cursors should never try and repeat the command signal -
  ;;   instead we manually inject the emulated command. To do this we force it
  ;;   to ignore our signal.

  ;; Note we demote errors so Voicemacs doesn't fall over completely if the
  ;; patch stops working.

  (defun voicemacs--patch-mc-lists (&rest _)
    "Remove the command signal from the multiple cursors lists."
    (with-demoted-errors "`voicemacs' error patching `multiple-cursors': %s"
      (add-to-list 'mc/cmds-to-run-once 'voicemacs--command-event-handler)
      ;; In case the user had it stored in the wrong place.
      (setq mc/cmds-to-run-for-all (remove 'voicemacs--command-event-handler
                                           mc/cmds-to-run-for-all))))
  (with-demoted-errors "`voicemacs' error patching `multiple-cursors': %s"
      (voicemacs--patch-mc-lists)
      ;; The list file can be lazy-loaded, so re-apply the patch then.
      (advice-add 'mc/load-lists :after 'voicemacs--patch-mc-lists)))


(defun voicemacs--command-event-handler ()
  "Handle a command emulation event.

This is made interactive so it can be bound to a signal - you
shouldn't call it yourself via `M-x' (or bind it to an actual
key)."
  (interactive)
  (let* ((command-event-data (cdr (aref (this-command-keys-vector) 0)))
         (command-name (nth 0 command-event-data))
         (prefix-arg (nth 1 command-event-data))
         ;; This is an RPC command. The user doesn't need to know the keyboard
         ;; shortcut, it's annoying.
         (suggest-key-bindings nil))
    ;; HACK: Compatibility with `multiple-cursors-mode'.
    (voicemacs--multiple-cursors-manual-injection
     command-name)

    ;; Usually this shouldn't be called from Elisp code, but we want Emacs to
    ;; treat this as a normal command as much as possible.
    (execute-extended-command prefix-arg (format "%s" command-name))))


;; In order to have voice commands act as normal commands, we bind a signal.
;;
;; We use `override-global-map' from `bind-key' to ensure this is always bound,
;; even in modes that unbind other keys. We aren't actually binding a key here -
;; we're binding a custom signal, which we'll emit manually.
(define-key override-global-map [voicemacs-command-signal] 'voicemacs--command-event-handler)


;; Ensure this command is callable via RPC
(voicemacs-expose-function 'voicemacs-inject-command)


(provide 'voicemacs-command)
;;; voicemacs-command.el ends here
