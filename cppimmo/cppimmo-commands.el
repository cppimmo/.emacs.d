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

(defun cppimmo/execute-command-other-frame (@command)
  "Run a command in a new frame.
@COMMAND ."
  (interactive "CC-x 5 M-x ")
  (select-frame (make-frame))
  (call-interactively @command))

(defun cppimmo/buffer-menu-other-frame ()
  "Execute the buffer-menu command in a new frame."
  (interactive)
  (cppimmo/execute-command-other-frame 'buffer-menu))

(defun cppimmo/bookmark-bmenu-other-frame ()
  "Execute the bookmark-bmenu-list command in a new frame."
  (interactive)
  (cppimmo/execute-command-other-frame 'bookmark-bmenu-list))
