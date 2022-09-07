
(defun cppimmo-count-words-buffer ()
  "Use the built-in count-words function to count the number of
words in a buffer."
  (interactive)
  (message "Word count (%s): %d" (buffer-name)
		   (count-words (point-min) (point-max))))

(defun cppimmo-count-words-region ()
  "Use the built-in count-words-region function to count the number
of words in a region."
  (interactive)
  (message "Word count region (%s): %d" (buffer-name)
		   (count-words-region (region-beginning) (region-end))))

;; cppimmo-count-words-mode-map defined automatically.
;; cppimmo-count-words-mode-hook defined lazily
(define-minor-mode cppimmo-count-words-mode
  "Use to easily count words within a buffer."
  :lighter " cppimmo-CW"
  :keymap (let ((map (make-sparse-keymap)))
			(define-key map (kbd "C-c C-b") 'cppimmo-count-words-buffer)
			(define-key map (kbd "C-c C-r") 'cppimmo-count-words-region)
			map))

