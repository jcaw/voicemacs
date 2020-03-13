(require 'company)
(require 'company-quickhelp)
(require 'cl)

(require 'voicemacs-base)
(require 'voicemacs-lib)


;; TODO: Sync this
(defun voicemacs-company-prompt-open? ()
  "Is the company prompt open?"
  (bound-and-true-p company-candidates))


(defun voicemacs-company-select-number (number)
  "Move selection to a numbered company candidate."
  (interactive "P")
  (company-set-selection (1- (+ number company-tooltip-offset))))


(defun voicemacs-company-pop-doc (number)
  (interactive "P")
  (voicemacs-company-select-number number)
  (company-quickhelp--show))


(voicemacs-expose-function 'company-complete-number)
(voicemacs-expose-function 'voicemacs-company-select-number)
(voicemacs-expose-function 'voicemacs-company-pop-doc)


(defvar voicemacs--current-company-number 1
  "The number of the last listed company candidate.")


;; TODO: PR `company-mode' for this functionality
(defun voicemacs-company-fill-propertize (original-function
                                          &optional value annotation
                                          width selected left right)
  "Intercept `company-fill-propertize' to number more candidates."
  ;; This is hard-coded to format correctly only with numbers up to double
  ;; digits.
  (when company-show-numbers
    (let ((original-number (string-to-number right)))
      ;; Get the new number
      (if (= 1 original-number)
          (setq voicemacs--current-company-number 1)
        (cl-incf voicemacs--current-company-number))
      ;; Segments that were un-numbered need to be narrowed to stop the prompt
      ;; distorting.
      (when (and (= original-number 0)
                 (not (= voicemacs--current-company-number 10)))
        (cl-decf width 2))
      ;; FIXME: cuts off last char of completion.
      (cl-decf width)
      ;; Create our own numbering and spacing
      (setq right (concat (voicemacs--pad-string (number-to-string
                                                  voicemacs--current-company-number)
                                                 3)
                          (company-space-string company-tooltip-margin)))))
  (funcall original-function value annotation width selected left right))


(defvar voicemacs--company-original-show-numbers nil
  "Stores the original value of `company-show-numbers'.

Used to store the value before
`voicemacs-company-more-numbers-mode' was activated, so it can be
restored when the mode is deactivated.")


(defun voicemacs--company-numbers-setup ()
  "Teardown for `voicemacs-company-more-numbers-mode'."
  (setq voicemacs--company-original-show-numbers
        (default-value company-show-numbers))
  (setq-default company-show-numbers t)
  (advice-add 'company-fill-propertize :around 'voicemacs-company-fill-propertize))


(defun voicemacs--company-numbers-teardown ()
  "Teardown for `voicemacs-company-more-numbers-mode'."
  (setq-default company-show-numbers voicemacs--company-original-show-numbers)
  (advice-remove 'company-fill-propertize 'voicemacs-company-fill-propertize))


(define-minor-mode voicemacs-company-more-numbers-mode
  "Hacks Company to number more than the first 10 candidates."
  :group 'voicemacs
  :global t
  :lighter nil
  :after-hook (if voicemacs-company-more-numbers-mode
                  (voicemacs--company-numbers-setup)
                (voicemacs--company-numbers-teardown)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun voicemacs-company-apply-recommended-defaults ()
  "Apply recommended default settings for `company-mode'."
  (setq-default company-show-numbers t
                company-tooltip-limit 30
                company-minimum-prefix-length 1))


(provide 'voicemacs-extend-company)
;;; voicemacs-extend-company ends here