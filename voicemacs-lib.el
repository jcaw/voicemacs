;; Utils library for Voicemacs.

;;; Code:


(require 'nav-flash)


(defun voicemacs--pad-string (string desired-length)
  "Pad left side of `STRING' to `DESIRED-LENGTH'."
  (concat (make-string
           ;; Don't want a negative length prefix.
           (max (- desired-length (length string))
                0)
           ? )
          string))


;; TODO: Convert these to question mark syntax
(defun voicemacs--bound-and-true-p (symbol)
  "Is `symbol' both bound, and truthy?

Like `bound-and-true-p', but a function, not a macro."
  (and (boundp symbol)
       (symbol-value symbol)))


(defun voicemacs-in-string-p ()
  "Returns t if currently in a string, nil otherwise.

`in-string-p' is obsolete so use this instead."
  (if (nth 3 (syntax-ppss)) t nil))


(defun voicemacs-assert-in (value valid-values)
  "Ensure a `VALUE' is one of `VALID-VALUES', with meaningful error."
  (unless (member value valid-values)
    (user-error "%s not one of %s" value valid-values)))


(defun voicemacs-flash (region-cons)
  "Flash a region in the buffer."
  (when region-cons
    ;; TODO: Don't use `nav-flash' for this. It doesn't block.
    (nav-flash-show (car region-cons) (cdr region-cons) 'isearch)))


(defun voicemacs-visual-copy (region-cons)
  (voicemacs-flash region-cons)
  (kill-ring-save (car region-cons) (cdr region-cons)))


(defun voicemacs-visual-kill (region-cons)
  (voicemacs-flash region-cons)
  (kill-region (car region-cons) (cdr region-cons)))


(defun voicemacs-visual-yank ()
  (let ((start-pos (point)))
    (yank)
    (voicemacs-flash (cons start-pos (point)))))


;; TODO: Smart insert, respecting e.g. whitespace.
(defun voicemacs-visual-insert (text)
  (let ((start-pos (point)))
    (insert text)
    (voicemacs-flash (cons start-pos (point)))))1


(defmacro voicemacs--first-result (result-form &rest body)
  "Perform `RESULT-FORM' then `BODY', but return the result of `RESULT-FORM'."
  (declare (indent 1))
  `(let ((result ,result-form))
     ,@body
     result))


(defun voicemacs--filter-atoms (func &optional obarray-)
  "Filter all atoms in `OBARRAY-' with `FUNC'.

Like with `mapatoms', `OBARRAY-' defaults to the value of
`obarray'."
  (let ((matches '()))
    (mapatoms (lambda (atom)
                (when (funcall func atom)
                  (push atom matches)))
              (or obarray- obarray))
    matches))


(defun voicemacs--maphash (func table)
  "Like `maphash', but returns the list of results like `mapcar'."
  (let ((results))
    (maphash (lambda (key value)
               (push (funcall func key value) results))
             table)
    results))


(defun voicemacs--set-to-list (set)
  "Convert `set' (represented by a hash table) to a list."
  (hash-table-keys set))


(defun voicemacs--set-add (element set)
  "Add `element' to `set' (represented by a hash table)."
  (unless (voicemacs--set-contains? element set)
    (puthash element t set)))


(defun voicemacs--set-contains? (element set)
  "Check if `element' is a member of `set' (represented by a hash table)."
  (gethash element set nil))


(defun voicemacs--set-remove (element set)
  "Remove `element' from `set' (represented by a hash table)."
  (remhash element set))


(provide 'voicemacs-lib)
;;; voicemacs-lib.el ends here
