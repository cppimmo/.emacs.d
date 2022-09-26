
;; Define the customize options file. System & configuration dependent.
(setq  custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Append my own Emacs Lisp library to the load-path variable.
(setq load-path (append load-path
						(list (concat user-emacs-directory "cppimmo"))))

(load "cppimmo-dvorak")
(load "cppimmo-xml")
(load "cppimmo-count-words-mode")

;;; PACKAGE SYSTEM SETUP ========================================================
;; Try to silence GPG errors on Windows.
(when (string-equal system-type "windows-nt")
  (setq package-check-signature nil))

(defun cppimmo/add-to-package-archives (@name @link)
  (add-to-list 'package-archives '(@name . @link) t))


;; Define and initialise package repositories.
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("melpa-stable" .  "http://stable.melpa.org/packages/") t)
(package-initialize)


;; Install use-package to simplify the configuration file.
(when (not (package-installed-p 'use-package))
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure 't)


(use-package gnu-elpa-keyring-update)


;;; BASIC STARTUP STUFF =========================================================
(progn
  (require 'subr-x)
  (if (string-equal system-type "windows-nt")
	  (setq user-full-name (getenv "USERNAME")))
;;  (if (string-equal system-type "gnu/linux")
;;	  (progn (shell-command "export EMACS_USER_FULL_NAME=` finger -s $USER \
;; | tr -s ' ' | cut -d ' ' -f 2,3 | tail -1`")
;;		(setq user-full-name (getenv "EMACS_USER_FULL_NAME"))))
  (setq user-mail-address
		(replace-regexp-in-string " dot " "."
								  (replace-regexp-in-string " at " "@"
															(concat (string-reverse "4240riuapffohb")
																	" at gmail dot com"))))
  (setq inhibit-startup-message t
		initial-scratch-message (concat "Welcome, " (capitalize user-login-name) "!")
		cursor-type 'bar))

;;; User Iterface
(defun cppimmo/configure-frame-size (@width @height)
  "Set the initial frame size for floating window managers.
@WIDTH the desired character width of the frame.
@HEIGHT the desired character height of the frame."
  (when window-system (set-frame-size (selected-frame) @width @height)))

(progn
  (add-to-list 'custom-theme-load-path "~/.emacs.d/cppimmo-themes/") ; Set theme load path.
  ;; Set the theme (if custom).
  (load-theme 'cppimmo-bright-ink t)

  ;; Extra highlighting for programming modes.
  (add-hook 'prog-mode-hook #'highlight-numbers-mode)
  (add-hook 'prog-mode-hook #'highlight-parentheses-mode)
  (add-hook 'prog-mode-hook #'show-paren-mode)
  (add-hook 'minibuffer-setup-hook #'highlight-parentheses-minibuffer-setup)

  (global-font-lock-mode t) ; Ensure syntax highlighting is always enabled.
  (setq font-lock-maximum-decoration t) ; Max font decor.
  
  (setq frame-title-format ; Set the frame title format.
		'("GNU Emacs - %b | " user-login-name "@" system-name))
  (setq tool-bar-mode nil) ; Disable icon tool bar.
  (setq column-number-mode t) ; Always show line cursor position in the modeline.
  (when (version<= "28.1" emacs-version)
	(setq display-time-mode t)) ; Display time in the modeline.
  (when (version<= "22" emacs-version)
	(setq display-battery-mode t))
  (when (version<= "26.0.50" emacs-version)
	(setq global-display-line-numbers-mode t)) ; Enable line number bar globally.
  (when (version<= "24.4" emacs-version)
	(setq global-visual-line-mode t) ; Enable visual line mode globally.
	(setq visual-line-fringe-indicators
		  '(left-curly-arrow right-curly-arrow))) ; Set the visual line fringe indicators.
  (if (string-equal system-type "windows-nt")
	  (progn (cppimmo/configure-frame-size 90 34)))
  ) ;; End of user interface settings.


;; Confirmation input settings.
(if (version< emacs-version "28.1")
	(defalias 'yes-or-no-p 'y-or-n-p-)
  (setq use-short-answers t)) ; I don't want to type yes or no each time.
;; Set the default directory (for find-file, etc.).
(setq default-directory user-emacs-directory)

(setq mouse-highlight t) ; The highlighting can white-out text on darker themes, enable it however.
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


;; Set the menu app key to hyper.
;; Use Shift+F10 or Ctrl+Shift+F10 instead for application/context menu.
;; Protesilaos blog post about setting the hyper key with xmodmap:
;; https://protesilaos.com/codelog/2019-10-10-debian-xmodmap/
(when (string-equal system-type "gnu/linux")
  (global-set-key (kbd "<menu>") nil))
(when (string-equal system-type "windows-nt")
  (setq w32-pass-apps-to-system nil)
  (setq w32-apps-modifier 'hyper))


;; Set binding for whitespace-mode minor mode.
(global-set-key (kbd "C-M-y") 'whitespace-mode)

;; Set binding for hl-line-mode.
(global-set-key (kbd "H-l b") 'hl-line-mode) ; locally.
(global-set-key (kbd "H-l g") 'global-hl-line-mode) ; Globally.

;; Set special bindings for the default nxml-mode.
(eval-after-load 'nxml-mode
  (lambda ()
	;; Set binding for CDATA tag insertion for XML documents.
	;;; See cppimmo/cppimmo-xml.el
	(define-key nxml-mode-map (kbd "C-c M-!") 'cppimmo/xml-insert-cdata)
	;; Set binding for blog insertion for XML documents.
	(define-key nxml-mode-map (kbd "C-c M-@") 'cppimmo/xml-insert-blog)
	;; Set binding for RSS feed item insertion for XML documents.
	(define-key nxml-mode-map (kbd "C-c M-#") 'cppimmo/xml-insert-blog-rss-item)))

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


(defun cppimmo/make-backup-file-name (@file-path)
  "This function from Xah Lee creates new directories for backups.
It creates directories that do not exist in the backup root.
Other methods of backup can easily exceed the MAX_PATH of POSIX systems."
  (let ($backup-root $backup-file-path) ; let* could be used here, but it would be ugly.
	(setq $backup-root "~/.emacs.d/backup/")
	;; This format remove the Windows drive letter.
	(setq $backup-file-path
		  (format "%s%s~" $backup-root
				  (replace-regexp-in-string "^[A-Za-z]:/" "" @file-path)))
	(make-directory
	 (file-name-directory $backup-file-path)
	 (file-name-directory $backup-file-path))
	$backup-file-path)) ; Return backup-file-path string.

(setq make-backup-files t) ; Make sure backups are enabled.
;; Set the backup file name function
(setq make-backup-file-name-function 'cppimmo/make-backup-file-name)


;; Preserve creation date on Windows (irrelevant on UNIX-like systems).
(if (string-equal system-type "windows-nt")
	(progn (setq backup-by-copying t)))
;; (setq create-lockfiles nil)
(setq auto-save-default nil) ; I save impulsively, so disabling this is fine.


;;; Enable disabled features.
(put 'upcase-region 'disabled nil) ; Upcase region C-x C-u
(put 'downcase-region 'disabled nil) ; Downcase region C-x C-l


;;; PACKAGE CONFIGURATION =======================================================

;; Settings for fill column indicator package. Toggle with "fci-mode".
(use-package fill-column-indicator)
(require 'fill-column-indicator)
(setq fci-rule-column 80)
(setq fci-rule-width 2)
(setq fci-rule-color "red")


;; Settings for the pomodoro package.
(use-package pomodoro)
(require 'pomodoro)
(pomodoro-add-to-mode-line) ; Add to modeline.
;;; Place all audio files in the repository to make things easier.
;; Work time alert.
(setq pomodoro-work-start-sound "~/.emacs.d/audio/bad-to-the-bone-fart.wav")
;; Break time alert.
(setq pomodoro-break-start-sound "~/.emacs.d/audio/bad-to-the-bone-fart.wav")
(defun cppimmo/play-pomodoro-sound (@sound)
  "Replace the play sound function for the pomodoro package."
  (play-sound-file (expand-file-name @sound)))
;; Properly replace the play sound function.
(advice-add 'play-pomodoro-sound :override #'cppimmo/play-pomodoro-sound)


;; Settings for the php mode package.
(use-package php-mode)


;; Settings for the lua mode package.
(use-package lua-mode)
(setq lua-indent-level 4)
(setq lua-indent-string-contents t)
(defun cppimmo/lua-mode-hook ()
  "I want tabs!"
  (setq indent-tabs-mode t) ; Enable indent tabs mode via the mode-hook.
  (abbrev-mode nil)) ; This probably isn't really relevant.

(add-hook 'lua-mode-hook 'cppimmo/lua-mode-hook)


;; Settings for the ox-leanpub package.
(use-package ox-leanpub)


;; Settings for the markdown mode package.
(use-package markdown-mode)
;; Settings for the markdown preview mode
(use-package markdown-preview-eww)

;; The the appropriate "markdown-command" for Microsoft Windows.
(if (string-equal system-type "windows-nt")
	(progn (custom-set-variables '(markdown-command "pandoc.exe"))))


;; Install the 2048-game package.
(use-package 2048-game)


;; Install the doom-themes packages and set the theme.
(use-package doom-themes)
;;  :config (load-theme 'doom-1337 t))


;; Install and configure SLIME.
(use-package slime)
(setq inferior-lisp-program "sbcl")


;; Install and configure magit.
(use-package magit)


;; Install and configure highlight numbers.
(use-package highlight-numbers)


;; Install and configure highlight parentheses.
(use-package highlight-parentheses)


;;; BUILT-IN MODE CONFIGURATION =================================================

(defun cppimmo/ispell-windows-nt ()
  "Configuration for the ispell functionality on Windows.
The trick is to use msys2 and the MinGW hunspell and hunspell-en packages.
 URL `https://www.reddit.com/r/emacs/comments/8by3az/how_to_set_up_sell_check_for_emacs_in_windows/'
 URL `https://stackoverflow.com/questions/8931580/hunspell-cant-open-affix-or-dictionary-files-for-dictionary-named-en-us'"
  (setq ispell-program-name "C:/tools/msys64/mingw64/bin/hunspell.exe") ; Set the executable name.
  (setq ispell-dictionary "en_US")) ; Set the appropriate word dictionary.
;; Set the ispell program name on Microsoft Windows systems.
(if (string-equal system-type "windows-nt")
    (progn (cppimmo/ispell-windows-nt))) ; Finally call the windows-nt configuration.

;; Configuration for the CC mode.
(setq c-default-style "bsd")
(setq-default c-basic-offset 4
	      tab-width 4
	      indent-tabs-mode t)
