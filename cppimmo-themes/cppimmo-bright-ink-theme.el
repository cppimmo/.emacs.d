;;; BSD 2-Clause License
;;; 
;;; Copyright (c) 2022, Brian Hoffpauir
;;; All rights reserved.
;;; 
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;; 
;;; 1. Redistributions of source code must retain the above copyright notice,
;;;    this list of conditions and the following disclaimer.
;;;     
;;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;;    this list of conditions and the following disclaimer in the documentation
;;;    and/or other materials provided with the distribution.
;;; 
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.
;;;
;;; Bright Ink Theme
;;;
(deftheme cppimmo-bright-ink
  "Bright ink theme for Emacs.
Consider installing highlight-numbers and highlight-parentheses.")

;;(let ((cppimmo-theme))
;;  (setq cppimmo-theme '())
;;  (apply #'custom-theme-set-faces cppimmo-theme))

(if (not (facep 'cppimmo/font-lock-operator-face))
	(defface cppimmo/font-lock-operator-face
	  '()
	  "Face for operator highlighting."
	  :group 'basic-faces))

;;(font-lock-add-keywords
;; 'c++-mode
;; '(("\\(~^&\|!<>:=,.\\+*/%-]\\)" 0 'cppimmo/font-lock-operator-face)))

;;; Operator Fonts
(defface cppimmo/font-lock-operator-face
 '((t (:foreground "#ff1210"))) "Basic face for operator." :group 'basic-faces)
;;; C-Like
(dolist (mode-iter '(c-mode c++-mode glsl-mode java-mode javascript-mode rust-mode))
  (font-lock-add-keywords mode-iter
   '(("\\([;~^&\|!<>=,.\\+*/%-]\\)" 0 'cppimmo/font-lock-operator-face keep))))
;;; Scripting
(dolist (mode-iter '(python-mode lua-mode))
  (font-lock-add-keywords mode-iter
   '(("\\([@~^&\|!<>:=,.\\+*/%-]\\)" 0 'cppimmo/font-lock-operator-face keep))))

(custom-theme-set-faces
 'cppimmo-bright-ink
 '(default ((t (:background "#ffffff" :foreground "#111111"))))
 '(cursor ((t (:background "#555577" :foreground "#ffffff"))))
 '(region ((t (:background "#919191"))))
 ;; Alternatively try:   :box '(:line-width 2 :color "#000000" :style bold)
 '(mode-line ((t (:background "#bfbfbf" :foreground "#000000" :box t))))
 '(mode-line-inactive ((t (:background "#e5e5e5" :foreground "#333333" :box t))))
 '(fringe ((t (:background "#bfbfbf"))))
 '(minibuffer-prompt ((t (:foreground "#ff2e00")))) ; :background "ffe900"
 '(font-lock-builtin-face ((t (:foreground "#3305A0"))))
 '(font-lock-comment-face ((t (:slant italic :foreground "#17A536"))))
 '(font-lock-constant-face ((t (:foreground "#17A536" :weight bold))))
 '(font-lock-function-name-face ((t (:foreground "#111111"))))
 '(font-lock-keyword-face ((t (:foreground "#3305A0"))))
 '(font-lock-string-face ((t (:foreground "#1508ff"))))
 '(font-lock-type-face ((t (:foreground "#3305A0"))))
 '(font-lock-variable-name-face ((t (:foreground "#111111"))))
 '(font-lock-warning-face ((t (:foreground "#ffb5d0" :weight bold))))
 '(erb-out-delim-face ((t (:foreground "#E6E1DC"))))
 '(erb-delim-face ((t (:foreground "#E6E1DC" ))))
 '(erb-exec-face ((t (:background "#191919"))))
 '(erb-out-face ((t (:background "#191919"))))
 '(isearch ((t (:background "#555555"))))
 '(lazy-highlight ((t (:background "#444444"))))
 '(link ((t (:foreground "#1f88ff" :underline t))))
 '(link-visited ((t (:foreground "#aaccff" :underline t))))
 '(button ((t (:background "#bfbfbf" :underline t))))
 '(header-line ((t (:background "#bfbfbf" :foreground "#000000" :box t))))
 '(line-number ((t (:background "#c5c5c5" :foreground "#111111")))) ; :box t
 '(hl-line ((t (:background "#ffe900" :weight bold))))
 '(highlight-numbers-number ((t (:foreground "#f213f3")))) ; highlight-numbers
 ;;'(error ((t (:foreground "#fc0202"))))
 ;;'(highlight-parentheses- ) ; hightlight-parentheses
 '(cppimmo/font-lock-operator-face ((t (:foreground "#ff1210"))))
 '(dired-directory ((t (:foreground "#3305a0"))))
 '(cppimmo/delim-face ((t (:foreground "#ff1210" :inherit shadow))))
 )

(provide-theme 'cppimmo-bright-ink)
