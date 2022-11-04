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
;;; init.el
;;
;; Define the customize options file. System & configuration dependent.
(setq  custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(defun cppimmo/append-to-load-path (@path &optional @use-dot-emacs)
  "Append @PATH string to the load-path variable.
@PATH Path string to append.
@USE-DOT-EMACS Prefix @PATH with user-emacs-directory when true."
  (when (not (stringp @path)) ; Ensure @PATH is a string.
	(error "Argument @PATH is not of type"))
  (setq load-path
		(append load-path
				(list
				 (if (equal @use-dot-emacs t)
					 ;; Prefix @PATH with user-emacs-directory.
					 (concat user-emacs-directory @path)
				   ;; No user-emacs-directory prefix.
				   @path)))))

(cppimmo/append-to-load-path "~/.emacs.d")
;; Append my own Emacs Lisp library directories to the load-path variable.
(mapcar (lambda (@path) ; Prefix @PATH with user-emacs-directory.
		  (funcall #'cppimmo/append-to-load-path @path t))
		(list "cppimmo" "addons"))
;; Load library files.
(load "cppimmo-dvorak")
(load "cppimmo-xml")
(load "cppimmo-count-words-mode")
(load "cppimmo-delim-face-mode")
(load "cppimmo-commands")

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
  (let (($str))
	(setq $str (concat (string-reverse "4240riuapffohb")
					     (replace-regexp-in-string " at " "@"
					       (replace-regexp-in-string " dot " "."" at gmail dot com"))))
	(setq user-mail-address $str)))

;;; User Iterface
(defun cppimmo/configure-frame-size (@width @height)
  "Set the initial frame size for floating window managers.
@WIDTH the desired character width of the frame.
@HEIGHT the desired character height of the frame."
  (when window-system (set-frame-size (selected-frame) @width @height)))

(defun cppimmo/initialize-font ()
  "Setup font."
  ;; Font stuff.
  (set-fontset-font
   t
   (if (version< emacs-version "28.1")
	   '(#x1f300 . #x1fad0) ; Then
	 'emoji) ; Else
   (cond
	((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
    ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
    ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
    ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
    ((member "Symbola" (font-family-list)) "Symbola"))))

(defun cppimmo/cycle-custom-themes ()
  "Cycle through the known custom themes."
  (interactive)
  (let (($known-themes)
		($active-theme))
	(setq $known-themes (cl-set-difference custom-known-themes
										   '(use-package user changed))
		  $active-theme (car custom-enabled-themes))
	(disable-theme $active-theme)
	(enable-theme (car $known-themes))))
  ;;(let (($index)
		;;($enabled-theme)
		;;($theme-at-index)
		;;($known-themes))
	;;(setq $known-themes (cl-set-difference custom-known-themes
	;;									   '(use-package user changed))
	;;	  $enabled-theme (car $known-themes)
	;;	  $index (cl-position $enabled-theme $known-themes))
	;;(message "Initial index: %d" $index)
	;;(catch 'cppimmo/cycle-custom-themes-break
	;;  (while (< $index (length $known-themes))
	;;	(setq $theme-at-index (nth $index $known-themes))
	;;	(message "Index: %d, Theme: %s" $index (symbol-name $theme-at-index))
	;;	(setq $index (1+ $index))))
	;;(message "Index (final): %d" $index)
	;;(load-theme (nth $index $known-themes) t)
	;;(disable-theme (nth $index $known-themes))
	;;(setq $index (1+ $index))
	;;(enable-theme (nth $index $known-themes))))

(progn
  (setq inhibit-startup-message t
		initial-scratch-message (concat "Welcome, " (capitalize user-login-name) "!"))
  
  (add-to-list 'custom-theme-load-path "~/.emacs.d/cppimmo-themes/") ; Set theme load path.
  ;; Set the theme (if custom).
  (load-theme 'cppimmo-bright-ink t)

  ;; Extra highlighting for programming modes.
  (add-hook 'prog-mode-hook #'highlight-numbers-mode)
  (add-hook 'prog-mode-hook #'highlight-parentheses-mode)
  (add-hook 'prog-mode-hook #'show-paren-mode)
  (add-hook 'minibuffer-setup-hook #'highlight-parentheses-minibuffer-setup)

  (setq cursor-type 'bar) ; Set the cursor type.
  
  (global-font-lock-mode t) ; Ensure syntax highlighting is always enabled.
  (setq font-lock-maximum-decoration t) ; Max font decor.
  
  (setq frame-title-format ; Set the frame title format.
		'("GNU Emacs - %b | " user-login-name "@" system-name))
  (tool-bar-mode -1) ; Disable icon tool bar.
  (column-number-mode t) ; Always show line cursor position in the modeline.
  (when (version<= "28.1" emacs-version)
	(setq display-time-24hr-format nil
		  display-time-default-load-average nil)
	(display-time-mode t)) ; Display time in the modeline.
  ;; Enable display-battery-mode if a battery exists.
  (when (and (version<= "22" emacs-version)
			 (not (string-match "N/A" (battery))))
	;; Example of battery output: "Power on-line, battery N/A (N/A% load, remaining time N/A)"
	(display-battery-mode t))
  (when (version<= "26.0.50" emacs-version)
	(global-display-line-numbers-mode t)) ; Enable line number bar globally.
  (when (version<= "24.4" emacs-version)
	(global-visual-line-mode t) ; Enable visual line mode globally.
	(setq visual-line-fringe-indicators
		  '(left-curly-arrow right-curly-arrow))) ; Set the visual line fringe indicators.
  (if (string-equal system-type "windows-nt")
	  (progn (cppimmo/configure-frame-size 90 34)))
  ;; Call font setup.
  (cppimmo/initialize-font)
  ) ;; End of user interface settings.

;;; Backup configuration.
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

(progn
  (setq make-backup-files t) ; Make sure backups are enabled.
  ;; Set the backup file name function
  (setq make-backup-file-name-function 'cppimmo/make-backup-file-name)
  )

;;; Miscellaneous
(progn
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
  
  ;; Set the fill column in auto fill mode.
  (add-hook 'text-mode-hook
			(lambda ()
			  ;; (turn-on-auto-fill) ; Keep as reference.
			  (set-fill-column 80)))
  
  (delete-selection-mode t) ; Enable overwriting of marked region.
  ;; (global-superword-mode t) ; Treat snake_case, etc. as a single word.
  (setq set-mark-command-repeat-pop nil) ; Disable mark popping (ex: C-u C-SPC).
  (setq mark-ring-max 10) ; 10 yanks limit.
  (setq global-mark-ring-max 10) ; 10 yanks limit.
  
  ;; Preserve creation date on Windows (irrelevant on UNIX-like systems).
  (if (string-equal system-type "windows-nt")
	  (progn (setq backup-by-copying t)))
  ;; (setq create-lockfiles nil)
  (setq auto-save-default nil) ; I save impulsively, so disabling this is fine.

  ;; I'm unsure if I should use this option, due to my use of cloud syncing clients
  ;; such as MEGA.  The file may be modified by MEGA and I could lose my work.
  ;; Although, this may be a moot concern.  Nevertheless, I will leave it incase I
  ;; believe I can use it in the future.
  ;; (global-auto-revert-mode 1) ; Reload buffer upon file modification.
  
  ;; Enable disabled features.
  (progn
	(put 'upcase-region 'disabled nil) ; Upcase region C-x C-u
	(put 'downcase-region 'disabled nil) ; Downcase region C-x C-l
	) ; End of enabling disabled features.
  ) ; End of miscellaneous.

;;; PACKAGE CONFIGURATION - NON-REPOS ===========================================
;; Install and configure glsl-mode.
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))

;;; PACKAGE CONFIGURATION - MY STUFF ============================================
;; Configure cppimmo/count-words-mode
(defun cppimmo/my-count-words-mode-hook ()
  (setq *cppimmo/count-words-use-header-line* nil))
(add-hook 'cppimmo/count-words-mode-hook #'cppimmo/my-count-words-mode-hook)

(cppimmo/global-delim-face-mode 1)

;;; PACKAGE CONFIGURATION - REPOS ===============================================
;; Settings for fill column indicator package. Toggle with "fci-mode".
(use-package fill-column-indicator
  :config
  (progn
	(setq fci-rule-column 80
		  fci-rule-width  2
		  fci-rule-color  "red")))

;; Settings for the pomodoro package.
(use-package pomodoro
  :config
  (progn
	(pomodoro-add-to-mode-line) ; Add to modeline.
	;; Place all audio files in the repository to make things easier.
	(setq pomodoro-work-start-sound
		  "~/.emacs.d/audio/beacon-alarm.wav") ; Work time alert.
	(setq pomodoro-break-start-sound
		  "~/.emacs.d/audio/beacon-alarm.wav") ; Break time alert.
	(defun cppimmo/play-pomodoro-sound (@sound)
	  "Replace the play sound function for the pomodoro package."
	  (play-sound-file (expand-file-name @sound)))
	;; Properly replace the play sound function.
	(advice-add 'play-pomodoro-sound :override #'cppimmo/play-pomodoro-sound)))

;; Install and configure php-mode
(use-package php-mode)

;; Install and configure lua-mode
(use-package lua-mode
  :config
  (progn
  (setq lua-indent-level 4)
  (setq lua-indent-string-contents t)
  (defun cppimmo/lua-mode-hook ()
	"I want tabs!"
	(setq indent-tabs-mode t) ; Enable indent tabs mode via the mode-hook.
	(abbrev-mode nil)) ; This probably isn't really relevant.
  (add-hook 'lua-mode-hook #'cppimmo/lua-mode-hook)))

;; Install and configure ox-leanpub
(use-package ox-leanpub)

;; Install and configure markdown-mode.
(use-package markdown-mode
  :config
  ;; The the appropriate "markdown-command" for Microsoft Windows.
  (if (string-equal system-type "windows-nt")
	  (progn (custom-set-variables '(markdown-command "pandoc.exe")))))
;; Install and configure markdown-preview-eww.
(use-package markdown-preview-eww)

;; Install the 2048-game package.
(use-package 2048-game)

;; Install the doom-themes packages and set the theme.
(use-package doom-themes)
;;  :config (load-theme 'doom-1337 t))

;; Install and configure SLIME.
(use-package slime
  :config
  (progn
	(setq inferior-lisp-program "sbcl"
		  slime-contribs '(slime-fancy
						   slime-company
						   slime-quicklisp slime-asdf))))
(use-package slime-company)

;; Install and configure magit.
(use-package magit)

;; Install and configure highlight numbers.
(use-package highlight-numbers)

;; Install and configure highlight parentheses.
(use-package highlight-parentheses)

;; Install and configure elfeed.
(use-package elfeed
  :config
  (progn
	(load "~/.emacs.d/cppimmo/cppimmo-feeds-pub.el")
	;; Load private feed file if it exists and append items to list.
	(let (($feed-file-priv "~/.emacs.d/cppimmo/cppimmo-feeds-priv.el"))
	  (when (file-exists-p $feed-file-priv)
		(load $feed-file-priv)))))

;; Install and configure ement.
;; (use-package ement)

;; Install and configure web-mode.
;; https://web-mode.org/
(use-package web-mode
  :config
  ;; TODO: Style left padding.
  (defun cppimmo/web-mode-hook ()
	"cppimmmo hook for web mode."
	(setq web-mode-markup-indent-offset 2 ; HTML indentation.
		  web-mode-css-indent-offset    2 ; CSS indentation.
		  web-mode-code-indent-offset   4 ; Code tags indentation.
		  web-mode-style-padding        1 ; Left padding relative to element.
		  web-mode-script-padding       1
		  web-mode-block-padding        1)
	(indent-tabs-mode -1) ; Turn off tab indentation.
	(add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
	(add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
	(add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
	(add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil)))
  (add-hook 'web-mode-hook #'cppimmo/web-mode-hook))

;; Install and confiure css-eldoc.
(use-package css-eldoc
  :config
  (add-hook 'css-mode-hook 'turn-on-css-eldoc)
  (add-hook 'scss-mode-hook 'turn-on-css-eldoc))

;; Install and configure helpful.
(use-package helpful)
;; helpful-callabe, -function, -macro, -command, -key, -variable, -at-point

;; Install and configure auctex.
;; (use-package auctex)

;; Install and configure powershell.
(use-package powershell)

;; Install and configure olivetti.
(use-package olivetti)

;; Install and configure pdf-tools.
;; (use-package pdf-tools)

;; Install and configure cursory.
(use-package cursory
  :config (cursory-set-preset
		   (or (cursory-restore-latest-preset) 'box)))

;; Install and configure cmake-mode.
(use-package cmake-mode)
;; Install and configure cmake-font-lock.
(use-package cmake-font-lock)
;; Install and configure cmake-ide.
(use-package cmake-ide)
;; Install and configure cmake-project.
(use-package cmake-project)

;; Install and configure org-journal.
(use-package org-journal
  :config (setq org-journal-dir "~/.emacs.d/org-journal"))

;; Install and configure ement.
(use-package ement)

;; Install and configure geiser.
(use-package geiser)
(use-package geiser-racket)
(use-package geiser-mit)
(when (string-equal system-type "gnu/linux") ; Prefer mit-scheme on GNU/Linux.
  (setq geiser-mit-binary "/usr/bin/scheme"
		geiser-active-implementations '(mit)))

;; Install and configure company.
(use-package company
  :config
  (setq company-idle-delay 0.15 ; Delay in seconds.
		company-minimum-prefix-length 3 ; Three characters minimum.
		company-selection-wrap-around t) ; Wrap compeletion dropdown navigation.
  (add-hook 'prog-mode-hook 'company-mode))

;; Install and configure nyan-mode.
(use-package nyan-mode
  :config
	;; (when (string-equal system-type "gnu/linux")
  ;; (nyan-mode 1))
  )

;; Install and configure info-colors.
(use-package info-colors
  :config
  (add-hook 'Info-selection-hook 'info-colors-fontify-node))

;; Install and configure paren-face.
(use-package paren-face)

;; Install and configure winum.
;; Select window <n> | C-x w <n>.
;; Select window greater than 10 | C-x w `.
(use-package winum
  :config
  (setq winum-scope 'frame-local
		winum-format "[%s]")
  (winum-mode))

;; Install and configure origami.
(use-package origami
  :config
  (add-hook 'prog-mode-hook #'global-origami-mode)
  (define-key origami-mode-map (kbd "M-o") #'origami-recursively-toggle-node))

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

;; Dired.
;; From: https://www.emacswiki.org/emacs/DiredSortDirectoriesFirst
(defun cppimmo/dired-sort ()
  "Make dired listings sort and display directories first."
  (save-excursion
	(let (buffer-read-only)
	  (forward-line 2) ;; Move beyond directory heading.
	  (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
	(set-buffer-modified-p nil)))

(defadvice dired-readin
	(after dired-after-updating-hook first () activate)
  "Make dired sort listings with directories first before adding marks."
  (cppimmo/dired-sort))

(defun cppimmo/dired-open-in-new-frame ()
  "Open a file/directory in a new frame."
  (interactive)
  (find-file-other-frame (dired-get-file-for-visit)))

;; ERC.
;; Set the ERC log directory. (C-c C-l)
(setq erc-server "irc.libera.chat"
      erc-nick "cppimmo"
      erc-user-full-name ""
      erc-track-shorten-start 8
      erc-autojoin-channels-alist '(("irc.libera.chat" "##slackware"))
      erc-kill-buffer-on-part t
      erc-auto-query 'bury
      erc-log-channels-directory "~/.emacs.d/erc-log")

(defun cppimmo/launch-erc ()
  ""
  (interactive)
  (erc-tls :port 6697))

;; Load abbreviations (abbrev-mode).
(load "~/.emacs.d/cppimmo/cppimmo-abbrev.el")

;; Bookmarks
(setq bookmark-save-flag 1 ; Save bookmark file after one modification.
	  ;; Ensure bookmark file default doesn't change.
	  bookmark-default-file "~/.emacs.d/bookmarks")

;; css-mode
(defun cppimmo/css-mode-hook ()
  "My css-mode hook."
  (setq-local css-indent-offset 2))
(add-hook 'css-mode-hook #'cppimmo/css-mode-hook)

;; rst-mode
(defun cppimmo/rst-mode-hook ()
  "My rst-mode hook."
  (setq-local fill-column 80)
  (auto-fill-mode t))
(add-hook 'rst-mode-hook #'cppimmo/rst-mode-hook)

;; Dairy.
(setq european-calendar-style nil)

;;; LOAD KEYBINDINGS ============================================================
(load "cppimmo-keybindings")
(cppimmo/bind-keys-g)
(cppimmo/bind-keys-m)
