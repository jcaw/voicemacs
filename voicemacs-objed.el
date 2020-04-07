(require 'objed)

(require 'voicemacs-base)


(defun voicemacs--objed-state (state combined-bounds)
  (voicemacs-assert-in state '(nil whole inner))
  (let ((bounds-list (if (eq state 'inner)
                         (nth 1 combined-bounds)
                       (nth 0 combined-bounds))))
    ;; Normalize to a cons cell
    (cons (nth 0 bounds-list) (nth 1 bounds-list))))


(defmacro voicemacs--with-objed-type (type &rest body)
  "Execute `body' with `objed-object' temporarily set to `type'"
  `(let ((objed--object ,type))
     ,@body))


(defun voicemacs-forward-objed-bounds (pos type &optional number state)
  (setq number (or number 1))
  (voicemacs--with-objed-type
   type
   (voicemacs--objed-state
    state
    (catch 'voicemacs-limit
      (let (bounds)
        (dotimes (i number bounds)
          (setq bounds (or (objed--get-next pos)
                           (throw 'voicemacs-limit bounds)))
          (setq pos (cdr (voicemacs--objed-state 'whole bounds)))
          bounds))))))


(defun voicemacs-backward-objed-bounds (pos type &optional number state)
  (setq number (or number 1))
  (voicemacs--with-objed-type
   type
   (voicemacs--objed-state
    state
    (catch 'voicemacs-limit
      (let (bounds)
        (dotimes (i number bounds)
            (setq bounds (or (objed--get-prev pos)
                                      (throw 'voicemacs-limit bounds)))
            (setq pos (car (voicemacs--objed-state 'whole bounds)))
            bounds))))))


;; TODO: Maybe remove
;;
;; (defun voicemacs--object-bounds (pos type &optional jumps state)
;;   (setq jumps (or jumps 0))
;;   (setq state (or state 'whole))
;;   (voicemacs-assert-in state '(whole inner))
;;   (cond ((= jumps 0)
;;          (objed-bounds-at pos type state))
;;         ((> jumps 0)
;;          (voicemacs-forward-objed-bounds pos type state))
;;         ((< jumps 0)  ;; Always t
;;          (voicemacs-backward-objed-bounds pos type state))))


(defun voicemacs--yank-bounds (bounds)
  (voicemacs-flash bounds)
  (kill-ring-save (car bounds) (cdr bounds)))


(defun voicemacs--kill-bounds (bounds)
  (voicemacs-flash bounds)
  (let ((start (car bounds))
        (end (cdr bounds)))
    (kill-ring-save start end)
    (delete-region start end)))


(defun voicemacs--mark-bounds (bounds)
  (push-mark (cdr bounds) nil t)
  (goto-char (car bounds)))


(defun voicemacs--move-start (bounds)
  (goto-char (car bounds)))


(defun voicemacs--move-end (bounds)
  (goto-char (cdr bounds)))


(defun voicemacs--maybe-intern (&rest symbols)
  (mapcar (lambda (symbol)
            ;; (let ((value (symbol-value symbol)))
            (when (stringp symbol)
              (set symbol (intern symbol))))
            ;; )
          symbols))


;; TODO: Remove
;; (defun voicemacs-object-action (action type &optional number state)
;;   ;; Allow strings to make wire transfer easier.
;;   (voicemacs--maybe-intern 'action 'type 'jumps 'state)
;;   (voicemacs--action-on-bounds
;;    action
;;    ;; TODO: Incorporate repeats of some kind?
;;    (or (voicemacs--object-bounds (point) type jumps state)
;;        (user-error (format "No `%s' `%s' found at point."
;;                            which type)))))


(defun voicemacs--assert-valid-action (action)
  (voicemacs-assert-in action '(yank kill mark move-start move-end)))


(defun voicemacs--do-direction-action (action adaptive-bounds target-bounds)
  (cond ((eq action 'yank) (voicemacs--yank-bounds adaptive-bounds))
        ((eq action 'mark) (voicemacs--mark-bounds adaptive-bounds))
        ((eq action 'kill) (voicemacs--kill-bounds adaptive-bounds))
        ((eq action 'move-start) (goto-char (car target-bounds)))
        ((eq action 'move-end) (goto-char (cdr target-bounds)))))


(defun voicemacs--do-action (action bounds)
  (voicemacs--do-direction-action action bounds))


(defun voicemacs-forward-action (action type number
                                 &optional next-state current-state)
  (voicemacs--maybe-intern action type number previous-state current-state)
  (voicemacs--assert-valid-action action)
  (let* ((point (point))
         (current-bounds (objed-bounds-at point type current-state))
         ;; Finding next object could be costly. Only need it when number > 0.
         (next-bounds (when (> 0 number)
                        (voicemacs-forward-objed-bounds
                         point type number next-state)))
         ;; Only used by some actions. Low overhead, compute here for neatness.
         (adaptive-bounds (if (= 0 number)
                              (cons point (cdr current-bounds))
                            (cons (car current-bounds) (cdr next-bounds)))))
    (voicemacs--do-direction-action action adaptive-bounds next-bounds)))


(defun voicemacs-backward-action (action type number
                                  &optional previous-state current-state)
  (voicemacs--maybe-intern action type number previous-state current-state)
  (voicemacs--assert-valid-action action)
  (let* ((point (point))
         (current-bounds (objed-bounds-at point type current-state))
         ;; Finding next object could be costly. Only need it when number > 0.
         (previous-bounds (when (> 0 number)
                            (voicemacs-backward-objed-bounds
                             point type number next-state)))
         ;; Only used by some actions. Low overhead, compute here for neatness.
         (adaptive-bounds (if (= 0 number)
                              (cons (car current-bounds) point)
                            (cons (car previous-bounds) (cdr current-bounds)))))
    (voicemacs--do-direction-action action adaptive-bounds previous-bounds)))


(defun voicemacs-object-action (action type &optional state)
  (voicemacs--assert-valid-action action)
  (voicemacs--do-action action (objed-bounds-at (point) type state)))


(defun voicemacs-strip-object (type)
  (let ((inner (objed-bounds-at-point type 'inner))
        (whole (objed-bounds-at-point type 'whole)))
    (delete-region (car whole) (car inner))
    (delete-region (cdr whole) (cdr inner))))


(defun voicemacs--join-bounds (type direction state)
  (cond ((or (not direction)
             (eq direction 'current))
         (objed-bounds-at-point type state))
        ((eq direction 'right)
         (voicemacs-forward-objed-bounds (point) type 1 state))
        ((eq direction 'left)
         (voicemacs-backward-objed-bounds (point) type 1 state))
        (t (error "Unrecognized direction: %s" direction))))


;; TODO: Pull this into current forward, backward actions


(defun voicemacs-join-objects (type left right)
  (voicemacs--maybe-intern type left right)
  (let* ((left-inner (voicemacs--join-bounds type left 'inner))
         (right-inner (voicemacs--join-bounds type right 'inner))
         ;; TODO: Innefficient to get left & right twice, we already get inner &
         ;;   outer during the first search, it's just pruned.
         (left-outer (voicemacs--join-bounds type left 'whole))
         (right-outer (voicemacs--join-bounds type right 'whole))
         (kill-start (cdr left-inner))
         (kill-end (car right-inner))
         (contained-newline (string-match-p (regexp-quote "\n")
                                            (buffer-substring-no-properties
                                             kill-start kill-end))))
    (goto-char kill-start)
    (voicemacs--kill-bounds (cons kill-start kill-end))
    (just-one-space -1)
    (when (and (not (voicemacs-in-string-p))
               contained-newline)
      (delete-char -1)
      (newline-and-indent)
      (goto-char kill-start))))


;; TODO: DWIM action - act on largest object at point, act on priority list of
;;   objects at point.


(mapc 'voicemacs-expose-function
      '(voicemacs-object-action
        voicemacs-forward-action
        voicemacs-backward-action
        voicemacs-strip-object
        voicemacs-join-objects))

;;; What do we need?

;; '(start/end object-type inner/outer action number)


(defun voicemacs--surrounds? (pos bounds &optional strict-start strict-end)
  (let ((start (car bounds))
        (end (cdr bounds)))
    (and (if strict-start (> pos start) (>= pos start))
         (if strict-end (< pos end) (<= pos end)))))


;; TODO: Revamp in & out of object, but don't do it just yet.

;; (defun voicemacs--backward-up-object (pos type &optional state)
;;   (while (not (voicemacs--surrounds? pos bounds))
;;     )
;;   (let ((pos-before (- (car (voicemacs--object-bounds pos type nil state))
;;                        1)))
;;     (or (and (>= pos-before 0)
;;              (voicemacs--object-bounds pos-before type nil state))
;;         (error "No outer object in backward direction."))))


;; (defun voicemacs--backward-up-bounds (pos type &optional state)
;;   ;; TODO: Should we just use a saved excursion for this?

;;   ;; TODO: Simplify. Horrible code.
;;   (catch 'voicemacs-result
;;     (let* (outer-bounds
;;            inner-bounds)
;;       (letf* (((symbol-function 'if-done-terminate)
;;                (lambda (outer-bounds inner-bounds)
;;                  (when (voicemacs--surrounds? pos (cons (car outer-bounds)
;;                                                         (cdr inner-bounds))
;;                                               t nil)
;;                    (throw 'voicemacs-result (if (eq state 'inner)
;;                                                 inner-bounds
;;                                               outer-bounds)))))
;;               ((symbol-function 'set-check-bounds)
;;                (lambda (check-pos)
;;                  (setq outer-bounds (objed-bounds-at check-pos type 'whole))
;;                  (setq inner-bounds (objed-bounds-at check-pos type 'inner))
;;                  (if-done-terminate outer-bounds inner-bounds))))
;;         (set-check-bounds pos)
;;         ;; TODO: Differentiate between original and target pos.
;;         (while (and inner-bounds outer-bounds)
;;           (setq outer-bounds (ignore-errors
;;                                (voicemacs-previous-objed-bounds
;;                                 (car outer-bounds) type 'whole)))
;;           ;; TODO: Maybe inefficient? Two backward lookups
;;           (setq inner-bounds (ignore-errors
;;                                (voicemacs-previous-objed-bounds
;;                                 (car outer-bounds) type 'inner)))
;;           (if-done-terminate outer-bounds inner-bounds))
;;         (mapc 'set-check-bounds
;;               (list check-pos (- check-pos 1)))
;;         (error "No outer object found.")))))


(defun voicemacs--forward-down-bounds (point this-inner-bounds next-inner-bounds)
  (let ((this-inner-start (car this-inner-bounds))
        (next-inner-start (car next-inner-bounds)))
    (cond ((> this-inner-start point) (goto-char this-inner-start))
          ((> next-inner-start point) (goto-char next-inner-start))
          (t nil))))


(defun voicemacs--backward-down-bounds (point this-inner-bounds last-inner-bounds)
  (let ((this-inner-end (cdr this-inner-bounds))
        (next-inner-end (cdr last-inner-bounds)))
    (cond ((< this-inner-end point) (goto-char this-inner-end))
          ((< next-inner-end point) (goto-char next-inner-end))
          (t nil))))


;; TODO: Aren't "into" and "out of" a lot clearer?
;; TODO: Down/up commands may not work if "next" produces the same object.

(defun voicemacs-forward-down (type &optional number)
  (dotimes (i (or number 1))
    (voicemacs--forward-down-bounds
     (point)
     (objed-bounds-at-point type 'inner)
     (voicemacs-forward-objed-bounds (point) type 1 'inner))))


(defun voicemacs-backward-down (type &optional number)
  (dotimes (i (or number 1))
    (voicemacs--backward-down-bounds
     (point)
     (objed-bounds-at-point type 'inner)
     (voicemacs-backward-objed-bounds (point) type 1 'inner))))


;; TODO study:
;; [x] How does forward into work with objed? - it doesn't
;; [ ] How does expanding/backwards out of work with objed?


;;; Misc TODOs

;; TODO: slurp & barf (probably need up/down for this to work)


(provide 'voicemacs-objed)
;;; voicemacs-objed.el ends here
