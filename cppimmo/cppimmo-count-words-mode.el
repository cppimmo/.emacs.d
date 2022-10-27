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
;;; Count Words Mode
;;
(defgroup cppimmo/count-words-mode
  nil ; Set MEMBERS later.
  "Custom group for cppimmo/count-words-mode.")

(defcustom *cppimmo/count-words-use-header-line* t
  "Enable/disable the word count for the buffer in the header line."
  :type 'boolean
  :group #'cppimmo/count-words-mode-custom-group)

(make-variable-buffer-local
 (defvar *cppimmo/count-words-mod-count* 0 ; Mod meaning modification.
   "Count the per buffer insertions to reduce word count frequency."))

(defcustom *cppimmo/count-words-mod-count-max* 25
  "Refresh the word count every mod-count-max insertions"
  :type 'integer
  :group #'cppimmo/count-words-mode-custom-group)

(defconst +cppimmo/count-words-avg-wpm+ 238
  "The average word per minutes of a reader.")

(defcustom *cppimmo/count-words-wpm* +cppimmo/count-words-avg-wpm+
  "The default word count if the user does not supply a custom value."
  :type 'integer
  :group #'cppimmo/count-words-mode-custom-group)

(defun cppimmo/count--lines (@minp @maxp)
  "Count the number of lines inclusively between @MINP and @MAXP."
  (count-lines @minp @maxp))

;; TODO: Ensure that this count is accurate and not affected by punctuations.
(defun cppimmo/count--words (@minp @maxp)
  "Count the number of words between @MINP and @MAXP"
  (catch 'count--words
  (save-excursion
	(let (($word-count 0))
	  (goto-char @minp)
	  (while (< (point) @maxp)
		(forward-word 1)
		(setq $word-count (1+ $word-count)))
	  ;; Use catch/throw to return the local word-count from the function.
	  (throw 'count--words $word-count)))))

(defun cppimmo/count-words-mode-post-mod-hook ()
  "Update the header line with a word count upon each insertion.
Mod meaning modification."
  (catch 'count-words-mode-post-mod-hook
	(when (eq *cppimmo/count-words-use-header-line* nil)
	  (throw 'count-words-mode-post-mod-hook nil))
	(setq *cppimmo/count-words-mod-count* (1+ *cppimmo/count-words-mod-count*))
	;; Only generate a new word count after mod-count-max modifications.
	(if (> *cppimmo/count-words-mod-count*
		   *cppimmo/count-words-mod-count-max*)
		(progn (setq header-line-format
					 (append (nbutlast header-line-format 1)
							 (list (format " Words: %d"
										   (cppimmo/count--words (point-min) (point-max))))))
			   (setq *cppimmo/count-words-mod-count* 0)))))
  
(defun cppimmo/count--characters (@minp @maxp)
  "Count the number of characters between @MINP and @MAXP."
  (- @maxp @minp))

(defun cppimmo/count--message (@minp @maxp)
  "Generate a message for @MINP and @MAXP."
  (message "Words(%s): %d, Characters: %d, Lines: %d"
			 (buffer-name)
			 (cppimmo/count--words @minp @maxp)
			 (cppimmo/count--characters @minp @maxp)
			 (cppimmo/count--lines @minp @maxp)))

(defun cppimmo/count-words-buffer ()
  "Count the number of words in a buffer."
  (interactive)
  (let (($minp (point-min)) ($maxp (point-max)))
	(cppimmo/count--message $minp $maxp))) ; Show message.

(defun cppimmo/count-words-region ()
  "Count the number of words in a region."
  (interactive)
  (let (($minp (region-beginning)) ($maxp (region-end)))
	(cppimmo/count--message $minp $maxp))) ; Show message.

(defun cppimmo/count-words-paragraph ()
  "Count the number of words in the paragraph under the point."
  (interactive)
  (let ($minp $maxp)
	(save-excursion ; Save point positions.
	  (backward-paragraph) ; Move backwards in current paragraph.
	  (setq $minp (point)) ; Save start of paragraph.
	  (forward-paragraph) ; Move forwards in current paragraph.
	  (setq $maxp (point))) ; Save end of paragraph.
	(cppimmo/count--message $minp $maxp))) ; Show message.

(defun cppimmo/count-words-line ()
  "Count the number of words in the current line under the point."
  (interactive)
  (let ($minp $maxp)
	(save-excursion ; Save point positions.
	  (beginning-of-visual-line) ; Move to the beginning of the current line.
	  (setq $minp (point)) ; Save the beginning of the line.
	  (end-of-visual-line) ; Move to the end of the current line.
	  (setq $maxp (point))) ; Save end of line.
	(cppimmo/count--message $minp $maxp))) ; Show message.

(defun cppimmo/count-words--read-time (@wpm @wc)
  "Base read time calculation based on words per minute.
@WPM words per minute.
@WC word count."
  (/ (float @wc) (float @wpm)))

;; TODO: Use prompt to display default value.
(defun cppimmo/count-words--read-time-prompt ()
  "Prompt meant to be supplied to the interactive function.
Requires a single @WPM argument."
  (list (read-number "Enter words per minute: " *cppimmo/count-words-wpm*)))

(defun cppimmo/count-words-region-read-time (@wpm)
  "Calculate an approximate read time for the marked region in minutes.
@WPM is the amount of words per minute that the user can read.
@WPM words per minute."
  (interactive (cppimmo/count-words--read-time-prompt))
  (message "~Read time: %.2f minutes"
		   (cppimmo/count-words--read-time @wpm
										   (cppimmo/count--words
											(region-beginning) (region-end)))))

(defun cppimmo/count-words-region-read-time-seconds (@wpm)
  "Calculate an approximate read time for the marked region in seconds.
@WPM is the amount of words per minute that the user can read.
@WPM words per minute."
  (interactive (cppimmo/count-words--read-time-prompt))
  (message "~Read time: %.2f seconds"
		   (* (cppimmo/count-words--read-time @wpm
											  (cppimmo/count--words
											   (region-beginning) (region-end))) 60.0)))

;; cppimmo/count-words-mode-map defined automatically.
;; cppimmo/count-words-mode-hook defined lazily
(define-minor-mode cppimmo/count-words-mode
  "Use to easily count words within a buffer."
  :lighter " Counter"
  :keymap (let (($map (make-sparse-keymap)))
			(define-key $map (kbd "C-c C-b") #'cppimmo/count-words-buffer)
			(define-key $map (kbd "C-c C-r") #'cppimmo/count-words-region)
			(define-key $map (kbd "C-c C-s p") #'cppimmo/count-words-paragraph)
			(define-key $map (kbd "C-c C-s l") #'cppimmo/count-words-line)
			(define-key $map (kbd "C-c C-t m") #'cppimmo/count-words-region-read-time)
			(define-key $map (kbd "C-c C-t s") #'cppimmo/count-words-region-read-time-seconds)
			$map)

  (if cppimmo/count-words-mode
	  (progn (message "cppimmo/count-words-mode activated!")
			 (add-hook 'post-self-insert-hook
					   #'cppimmo/count-words-mode-post-mod-hook))
	(progn (message "cppimmo/count-words-mode deactivated!")
		   (remove-hook 'post-self-insert-hook
						#'cppimmo/count-words-mode-post-mod-hook)
		   ;; Turn off the header line.
		   (setq header-line-format '()))))

;; Create and bind default hooks.
(defun cppimmo/count-words-mode-default-hook () nil)
(defun cppimmo/count-words-mode-default-on-hook () nil)
(defun cppimmo/count-words-mode-default-off-hook () nil)

(add-hook 'cppimmo/count-words-mode-hook
		  #'cppimmo/count-words-mode-default-hook)

(add-hook 'cppimmo/count-words-mode-on-hook
		  #'cppimmo/count-words-mode-default-on-hook)

(add-hook 'cppimmo/count-words-mode-off-hook
		  #'cppimmo/count-words-mode-default-off-hook)

(easy-menu-define cppimmo/count-words-menu cppimmo/count-words-mode-map
  "Menu for cppimmo/count-words-mode."
  '("Count Words"
	["Count Words in Buffer" cppimmo/count-words-buffer t]
	["Count Words in Region" cppimmo/count-words-region t]
	["Count Words in Paragraph" cppimmo/count-words-paragraph t]
	["Count Words in Line" cppimmo/count-words-line t]
	"---"
	["Calculate Region Read Time (Minutes)" cppimmo/count-words-region-read-time t]
	["Calculate Region Read Time (Seconds)" cppimmo/count-words-region-read-time-seconds t]))
