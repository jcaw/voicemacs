(require 'helm)


(defun voicemacs--helm-line-number ()
  "Line number of the current candidate."
  (with-helm-buffer (line-number-at-pos)))


(defun voicemacs-helm-goto-line (line &optional do-not-restore)
  "Go to the candidate on `line'.

`do-not-restore' is used for internal purposes. It suppresses
infinite loops on a recursive call."
  ;; TODO: This structure is gross. Rewrite it.
  (let ((start-line (voicemacs--helm-line-number)))
    (helm-beginning-of-buffer)
    (let ((last-line (voicemacs--helm-line-number)))
      (while (< (voicemacs--helm-line-number) line)
        (helm-next-line)
        (when (= (voicemacs--helm-line-number) last-line)
          ;; No more candidates.
          (unless do-not-restore
            (voicemacs--helm-goto-line start-line t))
          (error "Candidate does not exist"))
        (setq last-line (voicemacs--helm-line-number)))))
  (redisplay t))


(voicemacs-expose-function 'voicemacs-helm-goto-line)


(defun voicemacs--show-helm-numbers ()
  (when helm-alive-p
    (with-helm-buffer
      ;; TODO: Maybe make the face bolder
      (setq display-line-numbers 'absolute))))


(defun voicemacs--helm-numbers-mode-setup ()
  (cl-assert (boundp 'display-line-numbers) nil
             "`display-line-numbers' not available")
  (add-hook 'helm-update-hook 'voicemacs--show-helm-numbers))


(defun voicemacs--helm-numbers-mode-teardown ()
  (remove-hook 'helm-update-hook 'voicemacs--show-helm-numbers)
  ;; Also disable any active numbers
  (when helm-alive-p
    (with-helm-buffer
      (setq display-line-numbers nil))))


(define-minor-mode voicemacs-helm-numbers-mode
  "When active, helm will number candidates with absolute line numbers.

Incompatible with `helm-display-line-numbers-mode'."
  :group 'voicemacs
  :global t
  :lighter nil
  :after-hook (if voicemacs-helm-numbers-mode
                  (voicemacs--helm-numbers-mode-setup)
                (voicemacs--helm-numbers-mode-teardown)))


;; TODO: System for enabling by default?


(provide 'voicemacs-extend-helm)
;;; voicemacs-extend-helm.el ends here.