;; BSD 2-Clause License
;; 
;; Copyright (c) 2022, Brian Hoffpauir
;; All rights reserved.
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;; 
;; 1. Redistributions of source code must retain the above copyright notice, this
;;    list of conditions and the following disclaimer.
;;     
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;; Miscellaneous Commands
;;
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

(defun cppimmo/goto-percent (@percent)
  "Move to @PERCENT as a percentage of the buffer point position.
@PERCENT The percentage to move to in the buffer."
  (interactive "nGo to percent ([0, 100]%%): ")
  (when (not (and (>= @percent 0) (<= @percent 100)))
	(error "@PERCENT must be within the range: [0, 100]."))
  (goto-char (/ (* (point-max) @percent) 100)))
