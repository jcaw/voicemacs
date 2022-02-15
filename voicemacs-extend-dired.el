(require 'dired)
(require 'cl)

(require 'voicemacs-lib)
(require 'voicemacs-base)


(defvar-local voicemacs--dired-number-overlays '()
  "A list of currently active Dired number overlays")


(defface voicemacs-dired-numbers-face
  ;; '((t :inherit 'font-lock-keyword-face))
  ;; '((t :inherit 'font-lock-variable-name-face))
  '((t :inherit 'font-lock-type-face))
  "Face used for Dired selection numbers.")


(defun voicemacs--dired-remove-overlays ()
  "Remove all overlays in the current buffer."
  (mapc 'delete-overlay voicemacs--dired-number-overlays)
  (setq voicemacs--dired-number-overlays '()))


(defun voicemacs--dired-insert-number-overlay (number max-width)
  "Insert a single number overlay at the current position."
  (let ((overlay (make-overlay (point) (point))))
    ;; Add this property so we can find the candidate by number later.
    (overlay-put overlay 'number-value number)
    (overlay-put
     overlay
     'after-string
     (propertize (concat (voicemacs--pad-string (number-to-string number)
                                                max-width)
                         " ")
                 'face
                 '(voicemacs-dired-numbers-face default)))
    (push overlay voicemacs--dired-number-overlays)))


(defun voicemacs--dired-establish-width ()
  "Determine the maximum width needed for numbers."
  (save-excursion
    (goto-char 0)
    (let ((num-files 0))
      (while (zerop (forward-line))
        ;; We only want to label files & directories - not other lines.
        (when (dired-move-to-filename)
          (cl-incf num-files)))
      (length (number-to-string num-files)))))


(defun voicemacs--dired-insert-numbers (&rest _)
  "Number all candidates in the current buffer.

Resets numbers if they already exist. Will only work in Dired
buffers."
  (voicemacs--dired-remove-overlays)
  (let ((max-number-width (voicemacs--dired-establish-width))  ;; TODO: Find this dynamically?
        (current-number 1))
    (save-excursion
      (goto-char 0)
      ;; Forward-line returns 1 if it fails, 0 if it succeeds.
      (while (zerop (forward-line))
        ;; We only want to label files & directories - not the lines before.
        ;;
        ;; `dired-move-to-filename' will be truthy iff there's a file on the
        ;; line. (This also moves the point to the right position to insert
        ;; the number.)
        (when (dired-move-to-filename)
          (voicemacs--dired-insert-number-overlay
           current-number max-number-width)
          (cl-incf current-number))))))


;; TODO: Could make this more functional
(defun voicemacs--move-to-dired-item (number)
  "Move to a numbered dired item.

`NUMBER' - the number of the item."
  (unless voicemacs--dired-number-overlays
    (error "No numbered dired candidates available."))
  (catch 'item-found
    (mapc (lambda (overlay)
            (when (eq (overlay-get overlay 'number-value) number)
              (goto-char (overlay-start overlay))
              (dired-move-to-filename)
              (throw 'item-found t)))
          voicemacs--dired-number-overlays)
    (error "Dired item with this number was not found.")))


(defun voicemacs-dired-move-to-item (&optional item-number)
  "Move cursor to a numbered item in the dired buffer."
  (interactive "P")
  (assert (eq major-mode 'dired-mode))
  (voicemacs--move-to-dired-item item-number)
  ;; Ensure the move is shown to the user immediately.
  (redisplay t)
  ;; Dired does not respond to subsequent commands if they come too quickly, so
  ;; we sleep and redisplay.
  ;;
  ;; TODO: This might be a Windows-only issue.
  (sleep-for 0.1)
  (redisplay t))


(voicemacs-expose-function 'voicemacs-dired-move-to-item)


(defmacro voicemacs--dired-mapc (&rest body)
  "Run `BODY' within every dired buffer in turn."
  `(mapc (lambda (buffer)
           (with-current-buffer buffer
             (when (eq major-mode 'dired-mode)
               ,@body)))
         (buffer-list)))


(defun voicemacs--dired-renumber-buffer (buffer)
  "Insert numbers into a specific `BUFFER'."
  (when (bufferp buffer)
    (with-current-buffer buffer
      (voicemacs--dired-insert-numbers))))


(defun voicemacs--dired-queue-renumber ()
  "Queue a renumbering of the current buffer.

Defer to avoid duplicating work, and for compatibility with
certain dired rendering schemes (e.g. Doom Emacs hides \".\" &
\"..\" after candidates are inserted, but that won't remove their
number overlays)."
  (voicemacs--queue-once 'voicemacs--dired-renumber-buffer
                         :args (list (current-buffer))))


(defun voicemacs--dired-numbers-mode-setup ()
  (add-hook 'dired-after-readin-hook 'voicemacs--dired-queue-renumber)
  (voicemacs--dired-mapc
   (voicemacs--dired-insert-numbers)))


(defun voicemacs--dired-numbers-mode-teardown ()
  (remove-hook 'dired-after-readin-hook 'voicemacs--dired-queue-renumber)
  (voicemacs--dired-mapc
   (voicemacs--dired-remove-overlays)))


(define-minor-mode voicemacs-dired-numbers-mode
  "When active, `dired' candidates will be numbered."
  :group 'voicemacs
  :global t
  :lighter nil
  :after-hook (if voicemacs-dired-numbers-mode
                  (voicemacs--dired-numbers-mode-setup)
                (voicemacs--dired-numbers-mode-teardown)))


(provide 'voicemacs-extend-dired)
;;; voicemacs-extend-dired.el ends here
