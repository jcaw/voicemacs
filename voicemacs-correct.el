(require 'voicemacs-avy)


(defun voicemacs--bounds-of-subword-at-pos (pos)
  (save-excursion
    (goto-char pos)
    ;; FIXME: Make this always act on subwords irrespective of subword-mode.
    (or (bounds-of-thing-at-point 'word)
        (error "No word at pos: %s" pos))))


(defun voicemacs--correct-words (word1-pos word2-pos)
  "Correct words from `word1-pos' to `word2-pos'."
  (let ((word1-start (car (voicemacs--bounds-of-subword-at-pos word1-pos)))
        (word2-end (cdr (voicemacs--bounds-of-subword-at-pos word2-pos))))
    (voicemacs-flash (cons word1-start word2-end))
    (goto-char word1-start)
    ;; TODO: Prompt for correction? Maybe with helm prompt?
    (kill-region word1-start word2-end)))


(defun voicemacs-correct-words-avy (word1-start-char word2-start-char)
  "Correct all words between two words, identified by character."
  (interactive (list (read-char "First word start char: ")
                     (read-char "Second word start char: ")))
  (let* ((word1-pos (voicemacs-avy-pos (avy-goto-subword-1 word1-start-char)))
         (word2-pos (voicemacs-avy-pos (avy-goto-subword-1 word2-start-char)))
         (word1-window (alist-get 'window word1-pos)))
    ;; Check both matches are in the same window
    (unless (eq word1-window (alist-get 'window word2-pos))
      (error "Both matches must be in the same window."))
    (select-window word1-window)
    (voicemacs--correct-words (alist-get 'point word1-pos)
                              (alist-get 'point word2-pos))))


(defun voicemacs-correct-n-words-avy (n-words first-char)
  "Correct n words after a character with avy."
  (interactive (list (or current-prefix-arg 1)
                     (read-char "First char: ")))
  (voicemacs-avy-jump 'avy-goto-subword-1 first-char)
  (voicemacs--correct-words
   (point)
   (save-excursion
     (forward-word n-words)
     (point))))


(provide 'voicemacs-correct)
;;; voicemacs-correct.el ends here
