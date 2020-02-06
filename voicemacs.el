(defgroup voicemacs nil
  "Utilities to make Emacs easier to control by voice."
  :prefix "voicemacs-")


(defun voicemacs--mode-disable ()
  ;; TODO: Disable title modification
  ;; TODO: Stop syncer
  ;; TODO: Stop server
  )


(defun voicemacs--mode-enable ()
  ;; TODO: Start server
  ;; TODO: Start syncer
  ;; TODO: Enable title modification
  )


(define-minor-mode voicemacs-mode
  "Minor mode to communicate with voice recognition software."
  :group 'voicemacs
  :global t
  :lighter nil
  :after-hook (if voicemacs-mode
                  (voicemacs--mode-enable)
                (voicemacs--mode-disable)))


(provide 'voicemacs)
