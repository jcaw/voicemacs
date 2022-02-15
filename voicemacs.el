(require 'cl)
(require 'default-text-scale)

(require 'voicemacs-base)
(require 'voicemacs-command)

(require 'voicemacs-avy)

(require 'voicemacs-it)

;; TODO: Redo the way these extension modules are loaded.
(with-eval-after-load 'helm
  (require 'voicemacs-extend-helm))
(with-eval-after-load 'dired
  (require 'voicemacs-extend-dired))
(with-eval-after-load 'company
  (require 'voicemacs-extend-company))


(voicemacs-define-sync voicemacs
  :update t
  :enable nil
  :disable nil)


;; Major Mode Sync
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun voicemacs--mode-derivation-chain (mode)
  "Get a list of `mode' plus all derivation ancestors."
  (when mode
    (append (list mode)
            (voicemacs--mode-derivation-chain
             (get mode 'derived-mode-parent)))))


(voicemacs-define-sync-change-buffer major-mode-chain
  :update (voicemacs--mode-derivation-chain major-mode)
  :defer nil)

(voicemacs-define-sync-change-buffer primary-major-mode
  :update major-mode
  :defer nil)


;; Minor Modes Sync
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun voicemacs--active-minor-modes ()
  "Get the currently active minor modes."
  ;; Different packages register their minor modes in different lists, so we
  ;; have to iterate over both.
  (cl-union
   (seq-filter 'voicemacs--bound-and-true-p
               minor-mode-list)
   (seq-filter 'voicemacs--bound-and-true-p
               (mapcar #'car minor-mode-alist))))


(voicemacs-define-sync-change-buffer minor-modes
  :update (voicemacs--active-minor-modes)
  :defer t)


;; Defined Commands Sync
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun voicemacs--defined-commands ()
  "Get a list of all defined commands."
  ;; TODO: is there a dedicated variable for commands? Don't want to compute
  ;;   unless we have to.
  (voicemacs--filter-atoms 'commandp))


(defun voicemacs--temp-disable-name (func)
  "Helper for the `*-lazy-advise-defun' functions."
  (voicemacs--format-symbol "voicemacs--temp-disable-%s" func))


(defun voicemacs--lazy-advise-defun (func)
  "Advise `defun', but suppress the advice when entire files are loaded.

When loading many function definitions, we may not wish to run
the advice with every call to `defun'.

Advice will be run `:after' defun."
  (advice-add 'defun :after func)
  ;; Create a function to temporarily disable the advice we've given `defun'.
  ;; This function must be added as `:around' advice.
  (let ((temp-disable-func-name (voicemacs--temp-disable-name func)))
    (eval `(defun ,temp-disable-func-name (wrapped-func &rest args)
             (ignore-errors (advice-remove 'defun ,func))
             (voicemacs--first-result (apply wrapped-func args)
               (ignore-errors
                 (advice-add 'defun :after ,func)
                 ;; Since we suppressed it, we should manually queue once.
                 (,func)))))
    (advice-add 'require :around temp-disable-func-name)
    (advice-add 'load :around temp-disable-func-name)))


(defun voicemacs--undo-lazy-advise-defun (func)
  "Undo the effects of `voicemacs--lazy-advise-defun'."
  (let ((temp-disable-func-name (voicemacs--temp-disable-name func)))
    (advice-remove 'require temp-disable-func-name)
    (advice-remove 'load temp-disable-func-name)))


(voicemacs-define-sync defined-commands
  :update (voicemacs--defined-commands)
  :enable (voicemacs--lazy-advise-defun sync-func)
  :disable (voicemacs--undo-lazy-advise-defun sync-func)
  :defer t
  ;; Command definitions will be relatively rare after startup. Delay more than
  ;; normal to reduce visible overhead. Cheating, but user shouldn't notice.
  :delay 1)


;; Buffer name sync
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(voicemacs-define-sync-change-buffer buffer-name
  :update (buffer-name)
  :defer t)


;; Yasnippets Sync
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun voicemacs--snippet (template)
  "Make a voicemacs-style snippet from a yas `template'.

Returns a hash table with minimal information about the snippet,
to reduce the amount of data we have to sync."
  (let ((snippet (make-hash-table)))
    (puthash "name" (yas--template-name template) snippet)
    (puthash "key" (yas--template-key template) snippet)
    ;; TODO: Maybe allow spoken forms in snippets?
    snippet))


(defun voicemacs--snippets-from-table (table)
  "Get a list of voicemacs-style snippets from a `table'.

The structure (in JSON format) will be:

  [
    {\"name\": name, \"key\": key},
    {\"name\": name, \"key\": key},
    {\"name\": name, \"key\": key},
  ]

Each dict here represents one snippet."
  (let ((snippets '()))
    (maphash (lambda (template-key template)
               (push (voicemacs--snippet template) snippets))
             (yas--table-uuidhash table))
    snippets))


(defun voicemacs--get-snippets ()
  "Get all registered snippets in voicemacs format.

The structure (in JSON format) will be:

  {
    snippet-list-name: snippet-list,
    snippet-list-name: snippet-list,
  }

See `voicemacs--snippets-from-table' for the `snippet-list'
structure."
  (let ((snippets (make-hash-table)))
    (maphash (lambda (key table)
               (puthash key (voicemacs--snippets-from-table table) snippets))
             yas--tables)
    snippets))


(defun voicemacs-insert-snippet (snippet-name)
  (let ((where (if (region-active-p)
                   (cons (region-beginning) (region-end))
                 (cons (point) (point)))))
    (yas-expand-snippet
     (yas-lookup-snippet snippet-name)
     (car where) (cdr where))))


(with-eval-after-load 'yasnippet
  (voicemacs-define-sync-change-buffer active-yasnippet-tables
    :update (yas--modes-to-activate)
    :defer t)

  (voicemacs-define-sync yasnippets
    :update (voicemacs--get-snippets)
    :enable (progn
              (add-hook 'yas-after-reload-hook sync-func)
              ;; These seem to be the two lowest-level functions that are used
              ;; to add & remove (and update) snippets.
              (advice-add 'yas--add-template :after sync-func)
              (advice-add 'yas--remove-template-by-uuid :after sync-func))
    :disable (progn
               (remove-hook 'yas-after-reload-hook sync-func)
               (advice-remove 'yas--add-template sync-func)
               (advice-remove 'yas--remove-template-by-uuid sync-func))
    :defer t
    ;; This is an expensive operation, don't fire during every sit.
    :delay 0.1)

  (voicemacs-expose-function 'voicemacs-insert-snippet))


;; Org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(with-eval-after-load 'org
  (voicemacs-define-sync-change-buffer org-todo-keywords
    :update (if (boundp 'org-todo-keywords)
                (append org-todo-keywords
                        ;; Also add any buffer-local TODO keywords.
                        (when (bound-and-true-p org-todo-keywords-1)
                          (list (cons 'sequence org-todo-keywords-1)))))
    :defer t)

  (voicemacs-expose-function 'org-todo))


;; Cursor in a Comment?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun voicemacs-in-comment-p (&optional pos)
  "Check if the cursor is in a comment by examining font faces.

Uses current point by default. Provide `POS' to specify a
different position.

This function uses a similar method to that used by Flyspell."
  ;; `pos' defaults to point
  (unless (integerp pos)
    (setq pos (point)))
  ;; Check the face directly. Is it a comment face?
  (let* ((raw-faces (get-text-property pos 'face))
         ;; The 'face property could be a list, could be a single item.
         ;; Normalize it to a list.
         (faces-list (if (listp raw-faces)
                         raw-faces
                       (list raw-faces))))
    (or (member 'font-lock-comment-face faces-list)
        (member 'font-lock-comment-delimiter-face faces-list)
        (nth 4 (syntax-ppss)))))


;; TODO: Maybe also sync `in-string-p'?
(voicemacs-define-sync in-comment
  ;; Sending over the wire, so we need True or False, not truthiness
  :update (if (voicemacs-in-comment-p) t :json-false)
  ;; TODO: Maybe extract "register a sync that sycs immediately when idle" `'macro?
  :enable (run-with-idle-timer 0 0 sync-func)
  :disable (cancel-function-timers sync-func)
  ;; TODO: Forces regular syncs when we move cursor through a comment. can be
  ;;   slow, creates choppiness. Put this info in the title?
  :defer nil)


;; `evil-mode'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'evil
  (defvar voicemacs--evil-state-hooks '(evil-emacs-state-entry-hook
                                        evil-insert-state-entry-hook
                                        evil-motion-state-entry-hook
                                        evil-normal-state-entry-hook
                                        evil-operator-state-entry-hook
                                        evil-replace-state-entry-hook
                                        evil-visual-state-entry-hook)
    "Hooks that run on state entry for every default Evil state.")
  (voicemacs-define-sync evil-state
    :update (and evil-mode evil-state)
    :enable (progn
              (mapc (lambda (hook)
                      (add-hook hook sync-func))
                    voicemacs--evil-state-hooks)
              ;; In case the hooks don't catch it.
              (run-with-idle-timer 0 0 sync-func))
    :disable (progn
               (mapc (lambda (hook)
                       (remove-hook hook sync-func))
                     voicemacs--evil-state-hooks)
               (cancel-function-timers sync-func))
    :defer nil))


;; Text on Screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun voicemacs--visible-text ()
  "Get all visible text in each window.

Returns a list, each item is the visible text for one window."
  (remove nil
          (mapcar (lambda (window)
                    ;; Sometimes tries to gether from invalid windows. Just
                    ;; ignore those errors.
                    (ignore-errors
                      (with-selected-window window
                        (buffer-substring-no-properties (window-start) (window-end)))))
                  ;; All windows
                  (cl-mapcan #'window-list (frame-list)))))


(voicemacs-define-sync visible-text
  :update (voicemacs--visible-text)
  :enable (run-with-idle-timer 0.05 0 sync-func)
  :disable (cancel-function-timers sync-func)
  :defer nil)


;; Minibuffer Contents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(voicemacs-define-sync minibuffer-prompt
  :update (minibuffer-prompt)
  :enable (run-with-idle-timer 0 0 sync-func)
  :disable (cancel-function-timers sync-func)
  :defer nil)

;; Note that a prompt *will not* be reported when `current-message' is called.
;; Only `minibuffer-prompt' will get it.

(voicemacs-define-sync current-message
  ;; TODO: Crop extremely large current message?
  :update (let ((message- (current-message))
                ;; Restrict to ensure quick syncing
                (max-message-length 1000))
            (if (> (length message-) max-message-length)
                (substring message- 0 max-message-length)
              message-))
  :enable (run-with-idle-timer 0 0 sync-func)
  :disable (cancel-function-timers sync-func)
  :defer nil)


;; Active Symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defconst voicemacs-max-symbol-range-before (expt 10 6)
  "Max characters to scan after point when gathering symbols from buffer.")
(defconst voicemacs-max-symbol-range-after  (expt 10 6)
  "Max characters to scan before point when gathering symbols from buffer.")

(defvar voicemacs--active-symbols (make-hash-table)
  "Set of symbols that are \"present\" in the current context.")


(defun voicemacs--symbols-in-buffer ()
  "Get all symbols in the buffer.

Note this only searches within the range defined by
`voicemacs--max-symbol-range-before' and
`voicemacs--max-symbol-range-after'."
  ;; Only sync symbols for modes where it makes sense.
  ;;
  ;; TODO: Allow list of custom modes
  (save-excursion
    (let ((symbols (make-hash-table)))
      (goto-char (max (- (point) voicemacs-max-symbol-range-before)))
      (while (re-search-forward "[a-zA-Z0-9]"
                                voicemacs-max-symbol-range-after
                                t)
        (let* ((bounds (bounds-of-thing-at-point 'symbol)))
          (when bounds
            (goto-char (cdr bounds))
            ;; Only extract names from code
            (unless (or (voicemacs-in-string-p)
                        (voicemacs-in-comment-p))
              (voicemacs--set-add (buffer-substring-no-properties
                                   (car bounds) (cdr bounds))
                                  symbols)))))
      symbols)))


(defun voicemacs--sync-symbols (&rest _)
  ;; TODO: Allow this to cover multiple buffers. Segment by project.
  (when (derived-mode-p 'text-mode 'prog-mode)
    (setq voicemacs--active-symbols (voicemacs--symbols-in-buffer))))


(defun voicemacs--syncable-symbols (&rest _)
  ;; TODO: Extract probably from multiple buffers
  (voicemacs--set-to-list voicemacs--active-symbols))


;; TODO: Maybe re-enable symbols? Was failing in ein.
;; (voicemacs-define-sync active-symbols
;;   :update (progn (voicemacs--sync-symbols)
;;                  ;; TODO: Only sync if they've changed.
;;                  (voicemacs--syncable-symbols))
;;   :enable (progn (add-hook 'first-change-hook sync-func)
;;                  (voicemacs--hook-change-buffer sync-func))
;;   :disable (progn (remove-hook 'first-change-hook sync-func)
;;                   (voicemacs--unhook-change-buffer sync-func))
;;   :defer t
;;   :delay 0.1)


;; Emacs Metadata
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Whether it's Doom/Spacemacs shouldn't change after init - only need to check
;; once.
(defun voicemacs--check-distribution ()
  (voicemacs--update-if-changed
   'is-spacemacs
   (if (boundp 'spacemacs-version) t :json-false))

  ;; TODO: Untested. Checks this actually works with doom.
  (voicemacs--update-if-changed
   'is-doom
   (if (boundp 'doom-version) t :json-false)))

(add-hook 'after-init-hook 'voicemacs--check-distribution)
;; Check now in case the hook has been run.
(voicemacs--check-distribution)
;; Janky on Doom, just re-run it after startup.
(run-with-idle-timer 1 nil 'voicemacs--check-distribution)


;; Misc Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun voicemacs-increase-text ()
  (interactive)
  (default-text-scale-mode 1)
  (default-text-scale-increase))


(defun voicemacs-decrease-text ()
  (interactive)
  (default-text-scale-mode 1)
  (default-text-scale-decrease))


(defun voicemacs--yas-will-clear-field? ()
  "Is the point in a yas field that will be cleared on insert?"
  (let ((current-field (ignore-errors (yas-current-field))))
    (and current-field
         (not (yas--field-modified-p current-field))
         ;; TODO: Imperfect, if user has moved then returned to the start it
         ;; won't be erased on insert.
         (eq (point) (marker-position (yas--field-start current-field))))))


(cl-defun voicemacs-surrounding-text (&key (chars-before 30000)
                                           (chars-after 30000))
  "Get `num-chars' on each side of point.

If the point is in an unaltered yasnippet field, the field will
be altered as soon as the user starts typing - for this reason,
it is ignored. The text around the field will be returned."
  (let ((before-end (if (voicemacs--yas-will-clear-field?)
                        (marker-position (yas--field-start (yas-current-field)))
                      (point)))
        (after-start (if (voicemacs--yas-will-clear-field?)
                         (marker-position (yas--field-end (yas-current-field)))
                       (point))))
    `((text-before . ,(buffer-substring-no-properties
                       (max (point-min) (- before-end chars-before))
                       before-end))
      (text-after . ,(buffer-substring-no-properties
                      after-start
                      (min (point-max) (+ after-start chars-after)))))))


(voicemacs-expose-function 'voicemacs-surrounding-text)


(defun voicemacs-switch-to-minibuffer ()
  "Switch to minibuffer window (iff active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-frame-set-input-focus (window-frame (active-minibuffer-window)))
    (select-window (active-minibuffer-window))))


(defun voicemacs-toggle-region ()
  "Toggle whether the region is active or not."
  (interactive)
  (if (region-active-p)
      (progn
        (deactivate-mark t)
        (message "Mark deactivated.")
        t)
    (activate-mark)
    (message "Mark activated")
    nil))


;; TODO: Just sync `isearch-forward'?
(defun voicemacs-isearch-dwim (&optional prefix)
  "If isearching, repeat direction. Otherwise, start a forward isearch.

When starting a new search, passes the prefix to the underlying
isearch function."
  (interactive "P")
  (if isearch-mode
      ;; Call interactively so we can use command injection.
      (if isearch-forward
          (call-interactively 'isearch-repeat-forward)
        (call-interactively 'isearch-repeat-backward))
    (isearch-forward prefix)))


;; HACK: This can be used by the client to hold off on RPC calls until all
;;   existing input has been processed.
(defun voicemacs-input-pending? ()
  "Has all existing input been processed?"
  (if (input-pending-p) t :json-false))

(voicemacs-expose-function 'voicemacs-input-pending?)


(defun voicemacs-make-todo (keyword)
  "Turn the current line into a TODO item."
  (interactive "sTODO Keyword: ")
  (unless (voicemacs-in-comment-p)
    (comment-line nil))
  (goto-char (it-inner-start (it-object-at 'comment)))
  (insert keyword)
  (insert ": "))


(defmacro voicemacs-diff-indentation (&rest body)
  "Return the change in indentation at point after running `BODY'.

Only the line at point is tracked. The region is ignored."
  `(let ((indentation-before (current-indentation)))
     ,@body
     (- (current-indentation) indentation-before)))


(defun voicemacs-relative-indent (&optional amount)
  "Indent region `AMOUNT' backward from the automatic indentation level.

If region is not active, operates on the line.

This method indents the first line using
`indent-according-to-mode', then changes the indentation of the
rest of the block by the same amount. It then moves the block
back `AMOUNT' number of tab stops."
  (interactive "P")
  (setq amount (or amount 0))

  ;; Indent relative
  (if (use-region-p)
      ;; Suppress region deactivation
      (let (deactivate-mark)
        (save-excursion
          (let* ((end-marker (copy-marker (region-end)))
                 (difference (progn
                               (goto-char (region-beginning))
                               (deactivate-mark)
                               (voicemacs-diff-indentation
                                (indent-according-to-mode)))))
            ;; (echo (point))
            ;; (echo end-marker)
            ;; Skip first line - we already indented it.
            (forward-line 1)
            ;; Implementation adapted from `indent.el'
            (let ((pr (unless (minibufferp)
                        (make-progress-reporter "Indenting region..." (point) end-marker))))
              ;; (echo (point))
              ;; (echo end-marker)
              (while (< (point) end-marker)
                (or (and (bolp) (eolp))
                    (indent-line-to (+ (current-indentation) difference)))
                (forward-line 1)
                (and pr (progress-reporter-update pr (point))))
              (and pr (progress-reporter-done pr)))
            (activate-mark))))
    (indent-according-to-mode))
  ;; Now reduce indentation from relative point
  (voicemacs-indent-rigidly-left amount)
  )


(defun voicemacs-indent-rigidly-right (&optional times)
  "Indent right to the nearest tab stop(s)."
  (interactive "P")
  ;; Suppress region deactivation
  (let (deactivate-mark)
    (dotimes (i (or times 1))
      (if (use-region-p)
          (progn
            (indent-rigidly-right-to-tab-stop (save-excursion
                                                (goto-char (region-beginning))
                                                (deactivate-mark)
                                                (point-at-bol))
                                              (region-end))
            (activate-mark))
        (indent-rigidly-right-to-tab-stop (point-at-bol)
                                          (point-at-eol))))))


(defun voicemacs-indent-rigidly-left (&optional times)
  "Indent right to the nearest tab stop(s)."
  (interactive "P")
  ;; Suppress region deactivation
  (let (deactivate-mark)
    (dotimes (i (or times 1))
      (if (use-region-p)
          (progn
            (indent-rigidly-left-to-tab-stop (save-excursion
                                               (goto-char (region-beginning))
                                               (deactivate-mark)
                                               (point-at-bol))
                                             (region-end))
            (activate-mark))
        (indent-rigidly-left-to-tab-stop (point-at-bol)
                                         (point-at-eol))))))


;; Misc Exposed Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(voicemacs-expose-function 'x-focus-frame)


(defun voicemacs-find-file (path)
  "Like `find-file', but returns a JSON-encodable value & has no wildcards."
  (if (find-file path) t :json-false))
;; TODO: Don't like exposing `find-file' tbh
(voicemacs-expose-function 'voicemacs-find-file)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'voicemacs)
;;; voicemacs.el ends here
