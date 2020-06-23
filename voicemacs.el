(require 'cl)
(require 'default-text-scale)

(require 'voicemacs-base)
(require 'voicemacs-command)

(with-eval-after-load 'helm
  (require 'voicemacs-extend-helm))
(with-eval-after-load 'dired
  (require 'voicemacs-extend-dired))
(with-eval-after-load 'company
  (require 'voicemacs-extend-company))


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


(defun voicemacs--mapatoms (func &optional obarray-)
  (let ((matches '()))
    (mapatoms (lambda (atom)
                (when (funcall func atom)
                  (push atom matches)))
              (or obarray- obarray))
    matches))


(defun voicemacs--defined-commands ()
  "Get a list of all defined commands."
  ;; TODO: is there a dedicated variable for commands? Don't want to compute
  ;;   unless we have to.
  (voicemacs--mapatoms 'commandp))


;; TODO: Repeating concepts here. Turn this into a macro.
(defun voicemacs--sync-commands ()
  "Sync defined commands, iff they've changed."
  (voicemacs--update-if-changed 'defined-commands (voicemacs--defined-commands)))


(defun voicemacs--queue-sync-commands (&rest _)
  ;; Command definitions will be relatively rare after startup - we don't need
  ;; to update the list quickly. Delay more than normal to reduce visible
  ;; overhead.
  (voicemacs--queue-once 'voicemacs--sync-commands
                         :time 1))


(defun voicemacs--temp-disable-command-sync (original-func &rest args)
  "Temporarily stop syncing commands after every `defun'.

When loading many function definitions, we don't want to be
pushing the command sync onto a timer after every single `defun'.
We can use this to temporarily disable it, reducing overhead."
  ;; Ignore errors so we never affect/interrupt the underlying command - it
  ;; could be important, like `require' or `load'.
  (ignore-errors
    (advice-remove 'defun 'voicemacs--queue-sync-commands))
  (voicemacs--first-result (apply original-func args)
    (ignore-errors
      (advice-add 'defun :after 'voicemacs--queue-sync-commands)
      ;; Since we suppressed it, we should manually queue once.


(defun voicemacs--enable-sync-commands ()
  (advice-add 'defun :after 'voicemacs--queue-sync-commands)
  ;; When we load a file, there's lots of function definitions. Don't need to
  ;; sync after every one.
  (advice-add 'require :around 'voicemacs--temp-disable-command-sync)
  (advice-add 'load :around 'voicemacs--temp-disable-command-sync)
  ;; Sync current state immediately.
  (voicemacs--queue-sync-commands))


(defun voicemacs--disable-sync-commands ()
  (advice-remove 'defun 'voicemacs--queue-sync-commands)
  (advice-remove 'require 'voicemacs--remove-command-sync-advice)
  (advice-remove 'load 'voicemacs--remove-command-sync-advice))


(voicemacs--sync-add 'voicemacs--enable-sync-commands
                     'voicemacs--disable-sync-commands)


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

  (voicemacs-define-sync-change-buffer yasnippets
    :update (voicemacs--get-snippets)
    :defer t)

  (voicemacs-expose-function 'voicemacs-insert-snippet))


;; Org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun voicemacs--sync-org-todo (&rest _)
  "Synchronize the valid `org-todo' keywords."
  (voicemacs--update-if-changed
   'org-todo-keywords
   (if (boundp 'org-todo-keywords)
       (append org-todo-keywords
               ;; Also add any buffer-local TODO keywords.
               (when (bound-and-true-p org-todo-keywords-1)
                 (list (cons 'sequence org-todo-keywords-1)))))))


(defun voicemacs--queue-sync-org-todo (&rest _)
  "Queue a sync of the org TODO keywords."
  (voicemacs--queue-once 'voicemacs--sync-org-todo))


(defun voicemacs--enable-sync-org-todo ()
  "Enable synchronization of valid org TODO labels."
  (voicemacs--hook-change-buffer 'voicemacs--queue-sync-org-todo))


(defun voicemacs--disable-sync-org-todo ()
  "Disable synchronization of valid org TODO labels."
  (voicemacs--unhook-change-buffer 'voicemacs--queue-sync-org-todo))


(with-eval-after-load 'org
  (voicemacs--sync-add 'voicemacs--enable-sync-org-todo
                       'voicemacs--disable-sync-org-todo)
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
            (member 'font-lock-comment-delimiter-face faces-list)))
   ;; TODO: Fall back to the standard method if not?
   ;; (nth 4 (syntax-ppss))
   )


;; TODO: Forces regular syncs when we move cursor through a comment. That's
;;   slow, creates choppiness. Put this info in the title?
(defun voicemacs--sync-in-comment (&rest _)
  "Sync whether the cursor is in a comment."
  (voicemacs--update-if-changed
   'in-comment
   ;; Sending over the wire, so we need True or False, not truthiness
   (if (voicemacs-in-comment-p) t :json-false)))


(defun voicemacs--enable-sync-in-comment ()
  ;; Quickly sync whenever idle.
  (run-with-idle-timer 0 0 'voicemacs--sync-in-comment))


(defun voicemacs--disable-sync-in-comment ()
  (cancel-function-timers 'voicemacs--sync-in-comment))


(voicemacs--sync-add 'voicemacs--enable-sync-in-comment
                     'voicemacs--disable-sync-in-comment)


;; TODO: Maybe also in-string-p?


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


(defun voicemacs-isearch-forward (&optional prefix)
  "RPC isearch command. Start or repeat as needed.

Prefix will be passed to new search."
  (interactive "P")
  (if isearch-mode
      ;; Call interactively so we can use command injection.
      (call-interactively 'isearch-repeat-forward)
    (isearch-forward prefix)))


(defun voicemacs-isearch-backward (&optional prefix)
  "RPC isearch command. Start or repeat as needed.

Prefix will be passed to new search."
  (interactive "P")
  (if isearch-mode
      ;; Call interactively so we can use command injection.
      (call-interactively 'isearch-repeat-backward)
    (isearch-backward prefix)))


;; HACK: This can be used by the client to hold off on RPC calls until all
;;   existing input has been processed.
(defun voicemacs-input-pending? ()
  "Has all existing input been processed?"
  (if (input-pending-p) t :json-false))

(voicemacs-expose-function 'voicemacs-input-pending?)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'voicemacs)
;;; voicemacs.el ends here
