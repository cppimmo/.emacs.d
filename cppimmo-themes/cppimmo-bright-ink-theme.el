;;; Bright Ink Theme
;;
(deftheme cppimmo-bright-ink
  "Bright ink theme for Emacs.
Consider installing highlight-numbers and highlight-parentheses.")

;; (let ((cppimmo-theme))
;;   (setq cppimmo-theme '())
;;   (apply #'custom-theme-set-faces cppimmo-theme))

(if (not (facep 'cppimmo/font-lock-operator-face))
	(defface cppimmo/font-lock-operator-face
	  '()
	  "Face for operator highlighting."
	  :group 'basic-faces))

;; (font-lock-add-keywords
;; 'c++-mode
;; '(("\\(~^&\|!<>:=,.\\+*/%-]\\)" 0 'cppimmo/font-lock-operator-face)))

;; Operator Fonts
(defface cppimmo/font-lock-operator-face
 '((t (:foreground "#ff1210"))) "Basic face for operator." :group 'basic-faces)
;; C-Like
(dolist (mode-iter '(c-mode c++-mode glsl-mode java-mode javascript-mode rust-mode))
  (font-lock-add-keywords mode-iter
   '(("\\([;~^&\|!<>=,.\\+*/%-]\\)" 0 'cppimmo/font-lock-operator-face keep))))
;; Scripting
(dolist (mode-iter '(python-mode lua-mode))
  (font-lock-add-keywords mode-iter
   '(("\\([@~^&\|!<>:=,.\\+*/%-]\\)" 0 'cppimmo/font-lock-operator-face keep))))

(custom-theme-set-faces
 'cppimmo-bright-ink
 '(default ((t (:background "#ffffff" :foreground "#111111"))))
 '(cursor ((t (:background "#555577" :foreground "#ffffff"))))
 '(region ((t (:background "#919191"))))
 '(mode-line ((t (:background "#bfbfbf" :foreground "#000000"))))
 '(mode-line-inactive ((t (:background "#e5e5e5" :foreground "#333333"))))
 '(fringe ((t (:background "#000000"))))
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
 '(header-line ((t (:background "#e5e5e5" :foreground "#333333"))))
 '(line-number ((t (:background "#c5c5c5" :foreground "#111111"))))
 '(hl-line ((t (:background "#ffe900" :weight bold))))
 '(highlight-numbers-number ((t (:foreground "#f213f3")))) ; highlight-numbers
 ;; '(highlight-parentheses- ) ; hightlight-parentheses
 '(cppimmo/font-lock-operator-face ((t (:foreground "#ff1210"))))
 )

(provide-theme 'cppimmo-bright-ink)
