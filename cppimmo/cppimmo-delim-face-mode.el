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
(defgroup cppimmo/delim-face-mode
  nil ; Set MEMBERS later.
  "Custom group for cppimmo/count-words-mode.")

(defface cppimmo/delim-face '((t (:inherit shadow)))
  "Delimeter face for cppimmo/delim-face-mode."
  :group 'cppimmo/delim-face-mode)

(defcustom cppimmo/delim-face-regexp-lisp "[][()]"
  "Regular expression for lisp language delimeters."
  :type  'regexp
  :group 'cppimmo/delim-face-mode)

(defcustom cppimmo/delim-face-regexp-c "[][(){}]"
  "Regular expression for C-like language syntax delimeters"
  :type  'regexp
  :group 'cppimmo/delim-face-mode)

;; cppimmo/delim-face-mode-map defined automatically.
;; cppimmo/delim-face-mode-hook defined lazily
(define-minor-mode cppimmo/delim-face-mode
  "Use to create a face for programming language delimeters."
  :lighter ""
  :keymap (let (($map (make-sparse-keymap)))
			$map)
  
  (if cppimmo/delim-face-mode
	  (progn
		(message "cppimmo/count-words-mode activated!")
		(let ((keywords `((,cppimmo/delim-face-regexp 0 'cppimmo/delim-face))))
		  (if cppimmo/delim-face-mode
			  (font-lock-add-keywords  nil keywords)
			(font-lock-remove-keywords nil keywords)))
		(when font-lock-mode
		  (if (and (fboundp #'font-lock-flush)
				   (fboundp #'font-lock-ensure))
			  (save-excursion
				(widen)
				(font-lock-flush)
				(font-lock-ensure))
			(with-no-warnings
			  (font-lock-fontify-buffer)))))
	(progn
	  (message "cppimmo/count-words-mode deactivated!"))))

;;(define-globalized-minor-mode cppimmo/global-delim-face-mode
;;  cppimmo/delim-face-mode
;;  (lambda ()
;;	(cppimmo/delim-face-mode 1)
;;  :group cppimmo/delim-face-mode)

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
