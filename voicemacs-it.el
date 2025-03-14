(require 'voicemacs-base)
(require 'voicemacs-command)


(defvar voicemacs-it-type-keys
  '(name
    modes
    available-actions)
  "List of keys to synchronize from each object type.

These keys will be included when the object type is passed to the
client. Other keys will be discarded. This is used to limit the
information leaving Emacs.")


(defun voicemacs--it-filter-type (object-type keys)
  "Get a filtered variant of `object-type' with just `keys'."
  (-filter (lambda (property-pair)
             (member (car property-pair) keys))
           object-type))


(defun voicemacs--it-object-types ()
  "Get all available it object types.

The empty string will be used as the key for global objects,
rather than `nil'."
  (voicemacs--maphash
   (lambda (mode objects)
     (cons
       ;; Global objects are stored under the `nil' key, but we can't
       ;; encode a `nil' key into JSON. Replace it with `global'.
       (or mode 'global)
       ;; Simplify the objects, remove redundant information
       (voicemacs--maphash (lambda (name type)
                             ;; Just transfer the info we need
                             (voicemacs--it-filter-type type voicemacs-it-type-keys))
                           objects)))
   it-object-types))


(with-eval-after-load 'it
  ;; TODO: Maybe just expose all `it' actions?
  (voicemacs-expose-function 'it-wrap)
  (voicemacs-expose-function 'it-text-of-thing-at-dwim)

  (voicemacs-define-sync it-object-types
    :update (voicemacs--it-object-types)
    :enable (add-hook 'it-define-object-hook sync-func)
    :disable (remove-hook 'it-define-object-hook sync-func)
    :defer t
    ;; May be a big sync.
    :delay 0.1
    ))


(provide 'voicemacs-it)
;; voicemacs-it.el ends here
