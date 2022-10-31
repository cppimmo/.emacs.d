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
;; Paren-face-mode was used as a reference for implementing this code.
(defgroup cppimmo/delim-face-mode
  nil ; Set MEMBERS later.
  "Custom group for cppimmo/count-words-mode.")

(defface cppimmo/delim-face '((t (:inherit shadow)))
  "Delimeter face for cppimmo/delim-face-mode."
  :group 'cppimmo/delim-face-mode)

(defcustom *cppimmo/delim-face-modes-lisp*
  '(lisp-mode
	emacs-lisp-mode lisp-interaction-mode ielm-mode
	scheme-mode inferior-scheme-mode
	clojure-mode cider-repl-mode nrepl-mode
	arc-mode inferior-arc-mode)
  "List of major modes (Lisp languages) that dictate wether
`cppimmo/delim-face-mode' should be enabled."
  :type '(repeat symbol)
  :group 'cppimmo/delim-face-mode)

(defcustom *cppimmo/delim-face-regexp-lisp* "[][()]"
  "Regular expression for lisp language delimeters."
  :type  'regexp
  :group 'cppimmo/delim-face-mode)

(defcustom *cppimmo/delim-face-modes-c*
  '(c-mode objc-mode java-mode perl-mode awk-mode
	c++-mode javascript-mode sh-mode php-mode)
  "List of major modes (C-like languages) that dictate wether
`cppimmo/delim-face-mode' should be enabled."
  :type '(repeat symbol)
  :group 'cppimmo/delim-face-mode)

(defcustom *cppimmo/delim-face-regexp-c* "[][(){}]"
  "Regular expression for C-like language syntax delimeters"
  :type  'regexp
  :group 'cppimmo/delim-face-mode)

(defun cppimmo/delim-face-pre-turn-on (@regexp-sym)
  "Apply the font lock face to the appropriate delimeters defined
by @REGEXP-SYM.
@REGEXP-SYM A regular expression of syntax delimeters."
  (when (not (stringp @regexp-sym))
	(error "@REGEXP-SYM must be a string value."))
  (let* ((regexp-sym @regexp-sym) (keywords `((,regexp-sym 0 'cppimmo/delim-face))))
	(if cppimmo/delim-face-mode ; Check if cppimmo/delim-face-mode is activated.
		(font-lock-add-keywords  nil keywords) ; Add keywords to the font lock.
	  (font-lock-remove-keywords nil keywords))) ; Remove keywords to the font lock.
  (when font-lock-mode ; When font-lock-mode is activated.
	(if (and (fboundp #'font-lock-flush) ; Ensure each function is valid.
			 (fboundp #'font-lock-ensure))
		(save-excursion
		  (widen) ; Ensure access to all parts of the buffer.
		  (font-lock-flush) ; Declare font lock of the buffer parts as out-of-date.
		  (font-lock-ensure))
	  (with-no-warnings
		(font-lock-fontify-buffer))))) ; Reset font lock.

(defun cppimmo/delim-face-turn-on (&optional @use-global)
  "Perform initial setup for `cppimmo/delim-face-mode'.
The function determines if the current major mode is in the
`*cppimmo/delim-face-modes-lisp*' or `*cppimmo/delim-face-modes-c*' symbol lists and
applies the font face to the appropriate delimeters.
@USE-GLOBAL Optional boolean flag for use globally."
  (when (not (booleanp @use-global))
	(error "@USE-GLOBAL must be a boolean value."))
  (let ((apply-derived-mode-p
		 (lambda (@list) ; @LIST the arguments.
		   "Procedure to ensure the major modes specified in @LIST
are derived from the current major mode."
		   (apply #'derived-mode-p @list)))
		(modefun-if-global
		 (lambda ()
		   "Procedure to test if @USE-GLOBAL is true and enable the
minor mode globally."
		   (when @use-global
			   (cppimmo/delim-face-mode 1)))))
	(cond ((funcall apply-derived-mode-p *cppimmo/delim-face-modes-lisp*)
		   (cppimmo/delim-face-pre-turn-on *cppimmo/delim-face-regexp-lisp*)
		   (funcall modefun-if-global))
		  ((funcall apply-derived-mode-p *cppimmo/delim-face-modes-c*)
		   (cppimmo/delim-face-pre-turn-on *cppimmo/delim-face-regexp-c*)
		   (funcall modefun-if-global)))))

(defun cppimmo/global-delim-face-turn-on ()
  "Global version of `cppimmo/delim-face-turn-on'."
  (cppimmo/delim-face-turn-on t))

(define-minor-mode cppimmo/delim-face-mode
  "Use to create a font face for programming language delimeters."
  :lighter ""
  :keymap (let (($map (make-sparse-keymap)))
			$map)
  (if cppimmo/delim-face-mode
	  (progn
		(message "cppimmo/delim-face-mode activated!")
		(cppimmo/delim-face-turn-on))
	(progn
	  (message "cppimmo/delim-face-mode deactivated!"))))

(define-globalized-minor-mode cppimmo/global-delim-face-mode
  cppimmo/delim-face-mode ; The mode name.
  cppimmo/global-delim-face-turn-on ; Not a symbol?
  :group cppimmo/delim-face-mode)
  
;; Create and bind default hooks.
(defun cppimmo/delim-face-mode-default-hook () nil)
(defun cppimmo/delim-face-mode-default-on-hook () nil)
(defun cppimmo/delim-face-mode-default-off-hook () nil)

(add-hook 'cppimmo/delim-face-mode-hook
		  #'cppimmo/delim-face-mode-default-hook)
(add-hook 'cppimmo/delim-face-mode-on-hook
		  #'cppimmo/delim-face-mode-default-on-hook)
(add-hook 'cppimmo/delim-face-mode-off-hook
		  #'cppimmo/delim-face-mode-default-off-hook)

;;(easy-menu-define cppimmo/delim-face-mode-menu cppimmo/delim-face-mode-map
;;  "Menu for cppimmo/delim-face-mode."
;;  '("Hl Delim"
;;	["Count Words in Buffer" cppimmo/count-words-buffer t]))
