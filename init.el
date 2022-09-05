
;; Define the customize options file. System & configuration dependent.
(setq  custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Append my own Emacs Lisp library to the load-path variable.
(setq load-path (append load-path
			(list (concat user-emacs-directory "cppimmo"))))

(load "test.el")
(cppimmo-test)

;; PACKAGE SYSTEM SETUP =========================================================
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


;; BASIC STARTUP STUFF ==========================================================
(setq inhibit-startup-message t
	  initial-scratch-message "Welcome, Brian!"
	  cursor-type 'bar)

;; User Iterface
(tool-bar-mode -1) ; Disable icon tool bar.
(column-number-mode 1) ; Always show line cursor position in the modeline.
(when (version<= "28.1" emacs-version)
  (display-time-mode 1)) ; Display time in the modeline.
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode 1)) ; Enable line number bar globally.
(when (version<= "24.4" emacs-version)
  (global-visual-line-mode t) ; Enable visual line mode globally.
  (setq visual-line-fringe-indicators
		'(left-curly-arrow right-curly-arrow))) ; Set the visual line fringe indicators.

(defun cppimmo-configure-frame-size-windows-nt ()
  "Set the inital frame size of Microsoft Windows.
I don't use this for Linux, because I sometimes use window managers that do not
supported the typical floating layout."
  (when window-system (set-frame-size (selected-frame) 80 24)))
(if (string-equal system-type "windows-nt")
	(progn (cppimmo-configure-frame-size)))


;; Confirmation input settings.
(if (version< emacs-version "28.1")
	(defalias 'yes-or-no-p 'y-or-n-p-)
  (setq use-short-answers t)) ; I don't want to type yes or no each time.
;; Set the default directory (for find-file, etc.).
(setq default-directory user-emacs-directory)

;; (setq mouse-highlight nil)
(setq shift-select-mode t) ; I want to have select with shift + movement keys.
(setq line-move-visual t)

;; Remember the cursor position in previously visited files.
(if (version< emacs-version "25.0")
	(progn
	  (require 'saveplace)
	  (setq-default save-place t))
  (save-place-mode 1))


;; Enable overwriting of marked region.
(delete-selection-mode t)


;; Set binding for whitespace-mode minor mode.
(global-set-key (kbd "C-M-y") 'whitespace-mode)


(defun cppimmo-insert-xml-cdata ()
  "Insert CDATA tags for XML documents.
Moves the point back 3 characters for immediate editing."
  (interactive)
  (insert "<![CDATA[]]>")
  (let ((index 0))
	(while (< index 3)
	  (backward-char)
	  (setq index (+ index 1)))))
;; Set binding for CDATA tag insertion for XML documents.
(global-set-key (kbd "C-M-!") 'cppimmo-insert-xml-cdata)


;; Set the fill column in auto fill mode.
(add-hook 'text-mode-hook
		  (lambda ()
			;; (turn-on-auto-fill) ; Keep as reference.
			(set-fill-column 80)))


(setq set-mark-command-repeat-pop nil) ; Disable mark popping (ex: C-u C-SPC).
(setq mark-ring-max 10) ; 10 yanks limit.
(setq global-mark-ring-max 10) ; 10 yanks limit.

;; I'm unsure if I should use this option, due to my use of cloud syncing clients
;; such as MEGA.  The file may be modified by MEGA and I could lose my work.
;; Although, this may be a moot concern.  Nevertheless, I will leave it incase I
;; believe I can use it in the future.
;; (global-auto-revert-mode 1) ; Reload buffer upon file modification.


;; (setq make-backup-files nil)
;; (setq backup-by-copying t)
;; (setq create-lockfiles nil)
;; (setq auto-save-default nil)


;; PACKAGE CONFIGURATION ========================================================

;; Settings for fill column indicator package. Toggle with "fci-mode".
(use-package fill-column-indicator)
(require 'fill-column-indicator)
(setq fci-rule-column 80)
(setq fci-rule-width 2)
(setq fci-rule-color "red")

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
  (abbrev-mode nil)) ; This probably isn't really relevant.

(add-hook 'lua-mode-hook 'cppimmo-lua-mode-hook)


;; Settings for the ox-leanpub package.
(use-package ox-leanpub)


;; Settings for the markdown mode package.
(use-package markdown-mode)

;; The the appropriate "markdown-command" for Microsoft Windows.
(if (string-equal system-type "windows-nt")
	(progn (custom-set-variables '(markdown-command "pandoc.exe"))))


;; Install the 2048-game package.
(use-package 2048-game)


;; Install the doom-themes packages and set the theme.
(use-package doom-themes
  :config (load-theme 'doom-1337 t))


;; BUILT-IN MODE CONFIGURATION ==================================================

(defun cppimmo-ispell-windows-nt ()
  "Configuration for the ispell functionality on Windows.
The trick is to use msys2 and the MinGW hunspell and hunspell-en packages.
 URL `https://www.reddit.com/r/emacs/comments/8by3az/how_to_set_up_sell_check_for_emacs_in_windows/'
 URL `https://stackoverflow.com/questions/8931580/hunspell-cant-open-affix-or-dictionary-files-for-dictionary-named-en-us'"
  (setq ispell-program-name "C:/tools/msys64/mingw64/bin/hunspell.exe") ; Set the executable name.
  (setq ispell-dictionary "en_US")) ; Set the appropriate word dictionary.
;; Set the ispell program name on Microsoft Windows systems.
(if (string-equal system-type "windows-nt")
    (progn (cppimmo-ispell-windows-nt))) ; Finaly call the window-nt configuration.

;; Configuration for the CC mode.
(setq c-default-style "bsd")
(setq-default c-basic-offset 4
	      tab-width 4
	      indent-tabs-mode t)

