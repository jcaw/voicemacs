(require 'dired)
(require 'cl)

(require 'voicemacs-lib)
(require 'voicemacs-base)


(defvar-local voicemacs--dired-number-overlays '()
  "A list of currently active Dired number overlays")


(defface voicemacs-dired-numbers-face
  '((t :inherit 'font-lock-keyword-face))
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


(defun voicemacs--dired-numbers-mode-setup ()
  (advice-add 'dired-insert-set-properties :after 'voicemacs--dired-insert-numbers)
  (voicemacs--dired-mapc
   (voicemacs--dired-insert-numbers)))


(defun voicemacs--dired-numbers-mode-teardown ()
  (advice-remove 'dired-insert-set-properties 'voicemacs--dired-insert-numbers)
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


;; Custom Dired Commands
;; ---------------------


(defun voicemacs-create-new-file (file-list)
  ;; Originally written by Xah Lee
  (defun exsitp-untitled-x (file-list cnt)
    (while (and (car file-list)
                (not (string= (car file-list)
                              (number-to-string cnt))))
      (setq file-list (cdr file-list)))
    (car file-list))

  (defun exsitp-untitled (file-list)
    (while (and (car file-list) (not (string= (car file-list) "")))
      (setq file-list (cdr file-list)))
    (car file-list))

  (if (not (exsitp-untitled file-list))
      ""
    (let ((cnt 2))
      (while (exsitp-untitled-x file-list cnt)
        (setq cnt (1+ cnt)))
      (concat "" (number-to-string cnt) ""))))


(defun voicemacs-dired-create-file (file)
  (interactive
   (list (read-file-name "Create file: " (concat (dired-current-directory)
                                                 (voicemacs-create-new-file
                                                  (directory-files
                                                   (dired-current-directory)))))))
  (write-region "" nil (expand-file-name file) t)
  (dired-add-file file)
  (revert-buffer)
  (dired-goto-file (expand-file-name file)))


(defun voicemacs--open-files-external (file-list)
  "Open a list of files in an external app."
  ;; Originally written by Xah Lee
  (cond (voicemacs-on-windows
         (mapc (lambda (file-path)
                 (w32-shell-execute "open" (replace-regexp-in-string
                                            "/" "\\" file-path t t)))
               file-list))
        (voicemacs-on-mac
         (mapc (lambda (file-path)
                 (shell-command (format "open \"%s\"" file-path)))
               file-list))
        (voicemacs-on-linux
         (mapc (lambda (file-path)
                 (let ((process-connection-type nil))
                   (start-process "" nil "xdg-open" file-path)))
               file-list))))


(defun voicemacs-dired-open-in-external-app ()
  "Open all marked files in an external app.

If no files are marked, opens the current file.

If buffer is not in dired mode, just opens the current file."
  (interactive)
  ;; Originally written by Xah Lee
  (let* ((files (if (eq major-mode 'dired-mode)
                    (dired-get-marked-files)
                  (list (buffer-file-name))))
         (num-files (length files)))
    (when (or (<= file-list 5)
              (y-or-n-p (format "Open %s files? " num-files)))
      (voicemacs--open-files-external files))))


(defun voicemacs-dired-open-this-file-in-external-app ()
  "Open the current file only in an external app."
  (interactive)
  (voicemacs--open-files-external (list (dired-filename-at-point))))


(defun voicemacs-dired-show-in-desktop ()
  "Show current file in the OS's default file manager."
  (interactive)
  ;; Originally written by Xah Lee
  (cond
   (voicemacs-on-windows
    (w32-shell-execute "explore" (replace-regexp-in-string "/" "\\" default-directory t t)))
   (voicemacs-on-mac (shell-command "open ."))
   (voicemacs-on-linux
    (let ((process-connection-type nil)
          (openFileProgram (if (file-exists-p "/usr/bin/gvfs-open")
                               "/usr/bin/gvfs-open"
                             "/usr/bin/xdg-open")))
      (start-process "" nil openFileProgram "."))
    ;; (shell-command "xdg-open .") ;; 2013-02-10 this sometimes froze emacs till the folder is closed. ➢ for example: with nautilus
    )))


(defun voicemacs-dired-open-in-terminal ()
  "Open the current dir in a new terminal window."
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (message "Microsoft Windows not supported. File a bug report or pull request."))
   ((string-equal system-type "darwin")
    (message "Mac not supported. File a bug report or pull request."))
   ((string-equal system-type "gnu/linux")
    (let ((process-connection-type nil))
      (start-process "" nil "x-terminal-emulator"
                     (concat "--working-directory=" default-directory))))))


(defun voicemacs-dired-open-other-window-keep-focus ()
  "Keeping focus in Dired, open a file in the other window."
  (interactive)
  (let ((dired-window (frame-selected-window)))
    (dired-find-file-other-window)
    (select-window dired-window)))


(provide 'voicemacs-extend-dired)
;;; voicemacs-extend-dired.el ends here
