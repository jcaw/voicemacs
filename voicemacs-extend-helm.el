(require 'helm)

(require 'voicemacs-base)


;; TODO: Maybe check the nature of the current buffer? This seems janky.
;;
;; (eq (current-buffer)
;;     (with-helm-buffer (ignore-errors
;;                         (current-buffer))))
(voicemacs-define-sync-change-buffer in-helm-prompt
  :update (bound-and-true-p helm-alive-p)
  :defer nil)


;; The name of the current helm buffer
(voicemacs-define-sync-change-buffer helm-buffer-name
  :update (and (bound-and-true-p helm-alive-p)
               helm-last-buffer)
  :defer nil)


;; Hacky. Might not work.
(voicemacs-define-sync-change-buffer helm-title
  :update (when (bound-and-true-p helm-alive-p)
            (with-helm-buffer
              (buffer-substring-no-properties (line-beginning-position 0)
                                              (line-end-position 0))))
  :defer nil)


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


(defun voicemacs--local-override-face (face &rest overrides)
  "Override a face in the current buffer only."
  (setq-local face-remapping-alist
              (cons `(,face ,@overrides)
                    ;; Remove any overrides for this face that already exist.
                    (-filter (lambda (mapping)
                               (not (eq (car mapping) face)))
                             face-remapping-alist))))


(defun voicemacs--enable-selection-line-numbers (&rest _)
  "Make the current buffer show bold, absolute line numbers.

These numbers can be used for selection."
  ;; Default line numbers are subtle. These are selection numbers, make them
  ;; stand out.
  ;;
  ;; TODO: Extract line numbers face?
  (voicemacs--local-override-face 'line-number 'bold 'font-lock-keyword-face)
  (voicemacs--local-override-face 'line-number-current-line
                                  'bold
                                  'helm-selection
                                  'font-lock-keyword-face)
  (setq-local display-line-numbers 'absolute))


(defun voicemacs--show-helm-numbers ()
  (when helm-alive-p
    (with-helm-buffer
      (voicemacs--enable-selection-line-numbers))))


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; TODO: Maybe make a generalised function to invoke nonblocking RPC calls
(defun voicemacs-helm-swoop (query)
  "Run `helm-swoop' with a specific `query' on a timer.

Allows helm-swoop to be invoked via RPC without blocking."
  ;; Type check because it's delayed.
  (assert (stringp query))
  (run-with-timer 0 nil 'helm-swoop :query query))

(voicemacs-expose-function 'voicemacs-helm-swoop)


(defun voicemacs-helm-submit-if-single-match ()
  "Submit the match if only one exists."
  ;; Ensure matches are updated
  (when (helm--updating-p)
    (helm-refresh))
  ;; TODO: How to get currently matching candidates?
  (helm-candidate-number-limit)
  (helm-confirm-and-exit-minibuffer)
  )


(provide 'voicemacs-extend-helm)
;;; voicemacs-extend-helm.el ends here.
