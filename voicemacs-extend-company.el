(require 'company)
(require 'company-quickhelp)
(require 'cl)

(require 'voicemacs-base)
(require 'voicemacs-lib)


;; TODO: Maybe remove? Might be useful as an exposed method but not using it to
;;   sync.
(defun voicemacs-company-prompt-open? ()
  "Is the company prompt open?"
  (bound-and-true-p company-candidates))


(defconst voicemacs--company-prompt-key 'company-prompt-open
  "Voicemacs sync key to indicate whether the company prompt is open.")


(defun voicemacs--update-company-prompt-t (&rest _)
  (voicemacs--update-if-changed voicemacs--company-prompt-key t))


(defun voicemacs--update-company-prompt-false (&rest _)
  (voicemacs--update-if-changed voicemacs--company-prompt-key :json-false))


(defun voicemacs--enable-sync-company-prompt ()
  (add-hook 'company-completion-started-hook 'voicemacs--update-company-prompt-t)
  (add-hook 'company-after-completion-hook 'voicemacs--update-company-prompt-false)
  ;; Sync current state immediately.
  (company-cancel)
  (voicemacs--update-company-prompt-false))


(defun voicemacs--disable-sync-company-prompt ()
  (remove-hook 'company-completion-started-hook 'voicemacs--update-company-prompt-t)
  (remove-hook 'company-after-completion-hook 'voicemacs--update-company-prompt-false)
  (voicemacs--update-if-changed voicemacs--company-prompt-key :json-null))


;; This doesn't use the `it-define-sync' macro because it doesn't use a single
;; sync func.
(voicemacs--sync-add 'voicemacs--enable-sync-company-prompt
                     'voicemacs--disable-sync-company-prompt)


(defun voicemacs-company-highlight (number)
  "Move selection to a numbered company candidate."
  (interactive "p")
  (if (called-interactively-p 'any)
      (company-set-selection (+ company-tooltip-offset (- number 1)) t)
    ;; HACK: `company-set-selection' has to be called interactively, or the
    ;; selection won't show visually.
    (let ((current-prefix-arg number))
      ;; FIXME: This trick no longer works everywhere.
      (call-interactively 'voicemacs-company-highlight))))


(defun voicemacs-company-pop-doc (number)
  "Show the `company-quickdoc' for a numbered candidate."
  (interactive "p")
  (voicemacs-company-highlight number)
  (company-quickhelp--show))


(defun voicemacs-company-complete (number)
  "Insert a company candidate by `NUMBER'.

Like `company-complete-number', but gives visual feedback."
  (interactive "p")
  (voicemacs-company-highlight number)
  ;; Briefly hold with the candidate highlighted for visual feedback
  (sit-for 0.1)
  (company-complete))


(voicemacs-expose-function 'voicemacs-company-highlight)
(voicemacs-expose-function 'voicemacs-company-complete)
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
