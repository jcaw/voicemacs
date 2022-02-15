;; Commands & infrastructure for doing things with `avy'.


(require 'avy)

(require 'voicemacs-lib)


(defvar voicemacs-default-avy-jump-command 'avy-goto-subword-1
  "Default jump command to use with `voicemacs-avy-jump'.")


(defvar voicemacs-avy-keys "abcdefghijklmnopqrstuvwxyz'/#:\\-+_!£$*()[]{}"
  "Chars to use for Voicemacs avy jumps.

Note these will not override the default avy jump chars - only
jumps instigated with `voicemacs-avy-jump' will use these
characters.")


(defun voicemacs-avy-jump (avy-jump-command &rest avy-command-args)
  "Run an avy command, raising an error if the jump fails."
  ;; TODO: Maybe expand available symbols?
  (let* (
         ;; TODO: Is this the right place to ensure we're using old windows?
         (avy-all-windows t)
         ;; We're using voice, so there is no advantage to restricting to the
         ;; home row.
         (avy-keys (string-to-list voicemacs-avy-keys))
         (result (apply avy-jump-command avy-command-args)))
    ;; This means kill commands won't append, they'll create a fresh kill, but
    ;; that may have other beneficial side effects
    (setq last-command avy-jump-command)
    (cond ((eq t result) (error "No candidates found"))
          ((not result) (error "Jump cancelled"))
          (t result))))


(defmacro voicemacs-save-avy-excursion (avy-command &rest body)
  "Select location with `avy-command', perform `body', then restore point."
  ;; We use save excursion because `avy-pop-mark' doesn't tolerate buffer
  ;; changes before point.
  `(save-excursion
     ,avy-command
     ;; FIXME: Max sized ring will lose one element
     (voicemacs--first-result (progn ,@body)
       (avy-pop-mark))))


;; TODO: Overhaul this + uses, use my new macro
(defmacro voicemacs-avy-pos (avy-command)
  "Get the position of a character with Avy.

Returns a cons cell with the window & position of the match.

`(window . pos)'"
  `(voicemacs-save-avy-excursion
    ,avy-command
    `((point  . ,(point))
      (window . ,(selected-window)))))


(defmacro voicemacs-defavy (name revert-point-after docstring &rest after-jump-body)
  "Define a command that jumps, then iff successful, fires `AFTER-JUMP-BODY'.

You must provide a docstring for commands defined with this
macro."
  (declare (doc-string 3) (indent defun))

  ;; TODO: Optional docstring, intelligently check if one was provided.
  `(defun ,name (char)
     ,docstring
     (interactive (list (read-char "Char: ")))
     (,(if revert-point-after 'voicemacs-save-avy-excursion 'progn)
      (voicemacs-avy-jump voicemacs-default-avy-jump-command char)
      ,@after-jump-body)))


(voicemacs-defavy voicemacs-avy-copy nil
  "Jump, then copy the `it-thing' at point."
  (it-copy-dwim :flash t))




;; TODO: Jump back after this? Pad?
(voicemacs-defavy voicemacs-avy-kill nil
  "Jump, then kill the `it-thing' at point."
  (it-kill-dwim :flash t))


(voicemacs-defavy voicemacs-avy-mark nil
  "Jump, then mark the `it-thing' at point."
  (it-mark-dwim))


;; TODO: Switch to `it' for all flashing?
(voicemacs-defavy voicemacs-avy-yank nil
  "Jump, then yank the `it-thing' at point."
  (voicemacs-visual-yank :flash t))


(defun voicemacs-avy-teleport (char)
  (interactive (list (read-char "Char: ")))
  ;; TODO: Possible to combine both of these in one undo stage?
  (voicemacs-save-avy-excursion
   (voicemacs-avy-jump voicemacs-default-avy-jump-command char)
   (it-kill-dwim :flash t))
  (voicemacs-visual-insert (car kill-ring)))


(defun voicemacs-avy-duplicate (char)
  (interactive (list (read-char "Char: ")))
  ;; TODO: Possible to combine both of these in one undo stage?
  (voicemacs-save-avy-excursion
   (voicemacs-avy-jump voicemacs-default-avy-jump-command char)
   (it-copy-dwim :flash t))
  (voicemacs-visual-insert (car kill-ring)))



(defun voicemacs-avy-pad (char)
  "Apply `just-one-space' at `CHAR'."
  (interactive (list (read-char "Char: ")))
  (voicemacs-save-avy-excursion
   (voicemacs-avy-jump 'avy-goto-char char)
   (just-one-space)))


(defun voicemacs--jump-to-string (&optional string)
  "Jump to a multiple-char string. Signals an error if the jump fails."
  ;; TODO: If only one candidate, jump immediately.
  (voicemacs-avy-jump 'avy--generic-jump
                      (or string (regexp-quote (read-string "String: ")))
                      nil))


(defun voicemacs-avy-kill-word ()
  (interactive)
  ;; NOTE: The client needs to type fast for this to work.
  (voicemacs--jump-to-string)
  (it-kill-word))


(voicemacs-defavy voicemacs-avy-end-of-word-or-thing (char)
  "Jump to the end of the word at `char', or the `it-thing' if not a word."
  (goto-char (or (cdr (bounds-of-thing-at-point 'word))
                 (cdr (it-bounds (it-object-at 'sexp (point))))
                 (point))))


;; TODO: `voicemacs-avy-join'


(provide 'voicemacs-avy)
;;; voicemacs-avy.el ends here
