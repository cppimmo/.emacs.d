;;; Miscellaneous Commands
;; All interactive commands that don't belong to a mode or library.
(defun cppimmo/kill-ring-buffer-save ()
  "Place the entirety of the current buffer in the kill ring."
  (interactive)
  (save-excursion
    (push-mark (point)) ; Set mark at current point.
	(push-mark (point-min) nil t)
	(goto-char (point-max))
	(kill-ring-save (region-beginning) (region-end)))
  (message "Buffer saved to kill ring."))
