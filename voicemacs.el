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


(defun voicemacs--sync-major-mode (&rest _)
  "Sync the current major mode."
  (voicemacs--update-if-changed 'major-mode major-mode))


(defun voicemacs--enable-sync-major-mode ()
  (voicemacs--hook-change-buffer 'voicemacs--sync-major-mode)
  ;; Sync current state immediately.
  (voicemacs--sync-major-mode))


(defun voicemacs--disable-sync-major-mode ()
  (voicemacs--unhook-change-buffer 'voicemacs--sync-major-mode))


(voicemacs--sync-add 'voicemacs--enable-sync-major-mode
                     'voicemacs--disable-sync-major-mode)


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


(defun voicemacs--sync-minor-modes (&rest _)
  "Synchronize the active minor modes."
  (voicemacs--update-if-changed 'minor-modes (voicemacs--active-minor-modes)))


(defun voicemacs--queue-sync-minor-modes (&rest _)
  "Queue synchronization of active minor modes."
  (voicemacs--queue-once 'voicemacs--sync-minor-modes))


(defun voicemacs--enable-sync-minor-modes ()
  "Enable synchornization of the active minor modes."
  ;; TODO: Also want to sync change minor modes?
  (voicemacs--hook-change-buffer 'voicemacs--queue-sync-minor-modes))


(defun voicemacs--disable-sync-minor-modes ()
  "Disable synchornization of the active minor modes."
  (voicemacs--unhook-change-buffer 'voicemacs--queue-sync-minor-modes))


(voicemacs--sync-add 'voicemacs--enable-sync-minor-modes
                     'voicemacs--disable-sync-minor-modes)


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
  (let ((result (apply original-func args)))
    (ignore-errors
      (advice-add 'defun :after 'voicemacs--queue-sync-commands)
      ;; Since we suppressed it, we should manually queue once.
      (voicemacs--queue-sync-commands))
    result))


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


(defun voicemacs--sync-snippet-tables ()
  "Update voicemacs with the currently active snippet tables."
  (voicemacs--update-if-changed 'active-yasnippet-tables (yas--modes-to-activate)))


(defun voicemacs--queue-sync-snippet-tables (&rest _)
  "Queue an update to the currently active snippet tables."
  (voicemacs--queue-once 'voicemacs--sync-snippet-tables))


(defun voicemacs--sync-snippets ()
  "Update voicemacs with all registered snippets.

New snippets will only be pushed if they've changed. This
function can be slow, so don't run it regularly."
  ;; Compare JSON encodings because we don't know what types to expect -
  ;; difficult to compare equality. What matters are JSON forms. The question we
  ;; are asking is, "do we need to send new JSON data to the client?"
  (voicemacs--update-if-changed 'yasnippets (voicemacs--get-snippets)))


(defun voicemacs--queue-snippet-sync (&rest _)
  "Sync snippets on an idle timer.

Syncing snippets takes a long time so we generally want to do it
once, after all snippet updates have been applied and Emacs is no
longer busy."
  (voicemacs--queue-once 'voicemacs--sync-snippets))


(defun voicemacs--enable-sync-snippet-tables ()
  "Enable synchronization of the active yasnippet tables."
  ;; TODO: Queue this? May not be worth it, not a slow function.
  (voicemacs--hook-change-buffer 'voicemacs--queue-sync-snippet-tables))


(defun voicemacs--disable-sync-snippet-tables ()
  "Disable synchronization of the active yasnippet tables."
  (voicemacs--unhook-change-buffer 'voicemacs--queue-sync-snippet-tables))


(defun voicemacs--enable-sync-snippets ()
  "Enable synchronization of yasnippets."
  (add-hook 'yas-after-reload-hook 'voicemacs--queue-snippet-sync)
  ;; These seem to be the two lowest-level functions that are used to add &
  ;; remove (and update) snippets.
  (advice-add 'yas--add-template :after 'voicemacs--queue-snippet-sync)
  (advice-add 'yas--remove-template-by-uuid :after 'voicemacs--queue-snippet-sync)
  ;; Sync current state immediately
  (voicemacs--sync-snippets)
  (voicemacs--enable-sync-snippet-tables))


(defun voicemacs--disable-sync-snippets ()
  "Disable synchronization of yasnippets."
  (remove-hook 'yas-after-reload-hook 'voicemacs--queue-snippet-sync)
  (advice-remove 'yas--add-template 'voicemacs--queue-snippet-sync)
  (advice-remove 'yas--remove-template-by-uuid 'voicemacs--queue-snippet-sync)
  (voicemacs--disable-sync-snippet-tables))


(defun voicemacs-insert-snippet (snippet-name)
  (let ((where (if (region-active-p)
                   (cons (region-beginning) (region-end))
                 (cons (point) (point)))))
    (yas-expand-snippet
     (yas-lookup-snippet snippet-name)
     (car where) (cdr where))))


(with-eval-after-load 'yasnippet
  (voicemacs--sync-add 'voicemacs--enable-sync-snippets
                       'voicemacs--disable-sync-snippets)
  (voicemacs-expose-function 'voicemacs-insert-snippet))


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


;; Useful Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun voicemacs-increase-text ()
  (interactive)
  (default-text-scale-mode 1)
  (default-text-scale-increase))


(defun voicemacs-decrease-text ()
  (interactive)
  (default-text-scale-mode 1)
  (default-text-scale-decrease))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'voicemacs)
;;; voicemacs.el ends here
