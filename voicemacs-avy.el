;; Commands & infrastructure for doing things with `avy'.


(require 'avy)

(require 'voicemacs-lib)


(defun voicemacs-avy-jump (avy-jump-command &rest args)
  "Run an avy command, raising an error if the jump fails."
  ;; TODO: Maybe expand available symbols?
  ;;
  (let* (
         ;; TODO: Is this the right place to ensure we're using old windows?
         (avy-all-windows t)
         ;; We're using voice, so there is no advantage to restricting to the
         ;; home row.
         (avy-keys (string-to-list "abcdefghijklmnopqrstuvwxyz"))
         (result (apply avy-jump-command args)))
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


(defun voicemacs-avy-copy (char)
  (interactive (list (read-char "Char: ")))
  (voicemacs-save-avy-excursion
   (voicemacs-avy-jump 'avy-goto-subword-1 char)
   (it-copy-dwim :flash t)))


;; TODO: Jump back after this? Pad?
(defun voicemacs-avy-kill (char)
  (interactive (list (read-char "Char: ")))
  (voicemacs-avy-jump 'avy-goto-subword-1 char)
  (it-kill-dwim :flash t))


(defun voicemacs-avy-mark (char)
  (interactive (list (read-char "Char: ")))
  (voicemacs-avy-jump 'avy-goto-subword-1 char)
  (it-mark-dwim))


;; TODO: Switch to `it' for all flashing?
(defun voicemacs-avy-yank (char)
  (interactive (list (read-char "Char: ")))
  (voicemacs-avy-jump 'avy-goto-char char)
  (voicemacs-visual-yank :flash t))


(defun voicemacs-avy-teleport (char)
  (interactive (list (read-char "Char: ")))
  ;; TODO: Possible to combine both of these in one undo stage?
  (voicemacs-save-avy-excursion
   (voicemacs-avy-jump 'avy-goto-subword-1 char)
   (it-kill-dwim :flash t))
  (voicemacs-visual-insert (car kill-ring)))


(defun voicemacs-avy-duplicate (char)
  (interactive (list (read-char "Char: ")))
  ;; TODO: Possible to combine both of these in one undo stage?
  (voicemacs-save-avy-excursion
   (voicemacs-avy-jump 'avy-goto-subword-1 char)
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


;; TODO: `voicemacs-avy-join'


(provide 'voicemacs-avy)
;;; voicemacs-avy.el ends here
