
;; Define the customize options file. System & configuration dependent.
;; Try to place whatever options in this file.
(setq  custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))


;; Try to silence annoying GPG errors, because I don't really care.
(setq package-signature-check nil)


;; Define and initialise package repositories.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)


;; Install use-package to simplify the configuration file.
(when (not (package-installed-p 'use-package))
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure 't)


;; Basic Startup Stuff.

(setq inhibit-startup-message t
	  initial-scratch-message "Welcome, Brian!"
	  cursor-type 'bar)
;; Set the inital frame size of Microsoft Windows. I don't use this for Linux,
;; because I sometimes use window managers that do not supported the typical
;; floating layout.
(if (string-equal system-type "windows-nt")
	(progn
	  (when window-system (set-frame-size (selected-frame) 80 24))))


;; Disable tool bar.
(tool-bar-mode -1)

;; Always show line cursor position in the modeline.
(column-number-mode 1)

;; Display time in the modeline.
(display-time-mode 1)

;; Confirmation input settings.
;; (defalias 'yes-or-no-p 'y-or-n-p-) ; Disable for now.
;; Set the default directory (for find-file, etc.).
(setq default-directory user-emacs-directory)

;; Enable line number bar globally.
(global-display-line-numbers-mode 1)

;; Enable visual line mode globally.
(global-visual-line-mode t)

;; Set the visual line fringe indicators.
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;; Enable overwriting of marked region.
(delete-selection-mode t)


;; Set the fill column in auto fill mode.
(add-hook 'text-mode-hook
		  (lambda()
			;; (turn-on-auto-fill) ; Keep as reference.
			(set-fill-column 80)))


;; Settings for the pomodoro package.
(use-package pomodoro)
(require 'pomodoro)
(pomodoro-add-to-mode-line) ; Add to the modeline.


;; Settings for the php mode package.
(use-package php-mode)


;; Settings for the lua mode package.
(use-package lua-mode)
(setq lua-indent-level 4)
(setq lua-indent-string-contents t)
(defun cppimmo-lua-mode-hook ()
  "I want tabs!"
  (setq indent-tabs-mode t) ; Enable indent tabs mode via the mode-hook.
  (abbrev-mode nil) ; This probably isn't really relevant.
  )

(add-hook 'lua-mode-hook 'cppimmo-lua-mode-hook)


;; Settings for the ox-leanpub package.
(use-package ox-leanpub)


;; Settings for the markdown mode package.
(use-package markdown-mode)

;; The the appropriate "markdown-command" for Microsoft Windows.
(if (string-equal system-type "windows-nt")
	(progn
	  (custom-set-variables
	   '(markdown-command "pandoc.exe"))))


;; Install the 2048-game package.
(use-package 2048-game)


;; Install the doom-themes packages and set the theme.
(use-package doom-themes
  :config (load-theme 'doom-1337 t))


(defun cppimmo-ispell-windows-nt ()
  "Configuration for the ispell functionality on Windows.
 It truly is an unbearable experience. The trick is to use msys2
 and the MinGW hunspell and hunspell-en packages.
 URL `https://www.reddit.com/r/emacs/comments/8by3az/how_to_set_up_sell_check_for_emacs_in_windows/'
 URL `https://stackoverflow.com/questions/8931580/hunspell-cant-open-affix-or-dictionary-files-for-dictionary-named-en-us'"
  (setq ispell-program-name "hunspell.exe") ; Set the executable name.
  (setq ispell-dictionary "en_US") ; Set the appropriate word dictionary.
  )
;; Set the ispell program name on Microsoft Windows systems.
(if (string-equal system-type "windows-nt")
    (progn (cppimmo-ispell-windows-nt))) ; Finaly call the window-nt configuration.


;; Settings for fill column indicator package. Toggle with "fci-mode".
(use-package fill-column-indicator)
(require 'fill-column-indicator)
(setq fci-rule-column 80)
(setq fci-rule-width 2)
(setq fci-rule-color "red")

;; Configuration for the CC mode.
;; (setq-default whitespace-line-column 80
;;			  whitespace-style '(face lines-tail))
;; (add-hook 'cc-prog-mode-hook #'whitespace-mode)

;; (add-hook 'cc-mode-hook 'fci-mode)
(setq c-default-style "bsd")
(setq-default c-basic-offset 4
	      tab-width 4
	      indent-tabs-mode t)

