(require 'helm)

(require 'voicemacs-base)


(defun voicemacs-sync-helm-prompt (&rest _)
  "Sync whether the helm prompt is active."
  (voicemacs--update-if-changed
   'in-helm-prompt
   (bound-and-true-p helm-alive-p)))


(defun voicemacs-enable-sync-helm-prompt (&rest _)
  (voicemacs--hook-change-buffer 'voicemacs-sync-helm-prompt))


(defun voicemacs-disable-sync-helm-prompt (&rest _)
  (voicemacs--unhook-change-buffer 'voicemacs-sync-helm-prompt))


(voicemacs--sync-add 'voicemacs-enable-sync-helm-prompt
                     'voicemacs-disable-sync-helm-prompt)


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
          ;; No more candidates - restore original selection.
          (unless do-not-restore
            (voicemacs-helm-goto-line start-line t))
          (error "Candidate does not exist"))
        (setq last-line (voicemacs--helm-line-number)))))
  ;; Redisplay & pause so the user can see we moved the selection.
  (redisplay t)
  (sit-for 0.1))


(voicemacs-expose-function 'voicemacs-helm-goto-line)


(defun voicemacs--show-helm-numbers ()
  (when helm-alive-p
    (with-helm-buffer
      ;; TODO: Maybe make the face bolder
      (setq-local display-line-numbers 'absolute))))


(defun voicemacs--helm-numbers-mode-setup ()
  (cl-assert (boundp 'display-line-numbers) nil
             "`display-line-numbers' not available")
  (add-hook 'helm-update-hook 'voicemacs--show-helm-numbers))


(defun voicemacs--helm-numbers-mode-teardown ()
  (remove-hook 'helm-update-hook 'voicemacs--show-helm-numbers)
  ;; Also disable any active numbers
  (when helm-alive-p
    (with-helm-buffer
      (setq-local display-line-numbers nil))))


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
