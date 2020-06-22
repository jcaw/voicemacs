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


(defun voicemacs--bound-and-true-p (symbol)
  "Is `symbol' both bound, and truthy?

Like `bound-and-true-p', but a function, not a macro."
  (and (boundp symbol)
       (symbol-value symbol)))


(defmacro voicemacs--first-result (result-form &rest body)
  "Perform `RESULT-FORM' then `BODY', but return the result of `RESULT-FORM'."
  (declare (indent 1))
  `(let ((result ,result-form))
     ,@body
     result))


(provide 'voicemacs-lib)
;;; voicemacs-lib.el ends here
