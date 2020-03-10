;; Utils library for Voicemacs.

;;; Code:


(defun voicemacs--pad-string (string desired-length)
  "Pad left side of `STRING' to `DESIRED-LENGTH'."
  (concat (make-string
           ;; Don't want a negative length prefix.
           (max (- desired-length (length string))
                0)
           ? )
          string))


(provide 'voicemacs-lib)
;;; voicemacs-lib.el ends here
