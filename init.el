;;;; BSD 2-Clause License
;;;; 
;;;; Copyright (c) 2022, Brian Hoffpauir
;;;; All rights reserved.
;;;; 
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions are met:
;;;; 
;;;; 1. Redistributions of source code must retain the above copyright notice,
;;;;    this list of conditions and the following disclaimer.
;;;;     
;;;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;;;    this list of conditions and the following disclaimer in the documentation
;;;;    and/or other materials provided with the distribution.
;;;; 
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;;; POSSIBILITY OF SUCH DAMAGE.
;;;;
;;;; init.el
;;;;

;;; Define the customize options file. System & configuration dependent.
(progn
  (setq  custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
	(load custom-file)))

;;; System type control structure macro.  Use macroexpand to test.
(defmacro cppimmo:when-system (sys-symbols &rest body)
  "Control structure macro executes BODY when current system is one
of SYS-SYMBOLS.
SYS-SYMBOLS Can be a single symbol or list of symbols.

Example call with all possible SYS-SYSMBOLS:
(cppimmo:when-system '(windows linux bsd macos) ...)"
  (declare (indent 1) (debug ((symbolp form &optional form) body)))
  `(progn
	 (let* ((eval-symbols ,sys-symbols) ; Store evaluated macro argument
			(symbols eval-symbols)) ; Store evaluated macro argument in symbols list
	   ;; Convert single symbol to list if needed
	   (unless (listp eval-symbols)
		 (setq symbols (list eval-symbols)))
	   ;; Build up result of applying logical OR to each symbol in the SYMBOLS list
	   (when (seq-reduce
			  #'(lambda (a b) (or a b))
			  (mapcar (lambda (sym) ; This lambda relates a symbol to the corresponding SYSTEM-TYPE
						(cond
						 ((eq sym 'windows) (string-equal system-type "windows-nt"))
						 ((eq sym 'linux) (string-equal system-type "gnu/linux"))
						 ((eq sym 'bsd) (string-equal system-type "berkley-unix"))
						 ((eq sym 'macos) (string-equal system-type "darwin"))))
					  symbols)
			  nil) ; Supplying nil as the INITIAL-VALUE ensures the correct behavior
		 (progn ,@body)))))

(defun cppimmo:append-to-load-path (path &optional use-dot-emacs-p)
  "Append PATH string to the load-path variable.
PATH Path string to append.
USE-DOT-EMACS Prefix PATH with user-emacs-directory when true."
  (when (not (stringp path)) ; Ensure PATH is a string.
	(error "Argument PATH is not of type"))
  (setq load-path
		(append load-path
				(list
				 (if (equal use-dot-emacs-p t)
					 ;; Prefix PATH with user-emacs-directory.
					 (concat user-emacs-directory path)
				   ;; No user-emacs-directory prefix.
				   path)))))

(defun cppimmo:load-directory (path)
  "Load the Emacs Lisp source/binary files located in PATH."
  (message "Loading elisp files in %s" path)
  (let* ((elisp-pattern "\\(\\.el\\|\\.elc\\)$") ; Pattern for elisp file extensions
		 (abs-path-p t) ; Use absolute path
		 (elisp-files (directory-files path abs-path-p elisp-pattern)))
	(dolist (file elisp-files)
	  (message "Loading elisp file: %s" file)
	  (load file))))

;;; PACKAGE SYSTEM SETUP ========================================================

(defun cppimmo:internet-connection-p (&optional urls)
  "Test if an internet connection is available.
URLS List of URL strings (set to package-archives by default)."
  (unless urls (setq urls (mapcar 'cdr package-archives))) ; Use second element of pair
  (catch 'ret
    (dolist (url urls)
      (condition-case var
		  ;; kill-buffer silences output of url-retrieve-synchronously function
		  (kill-buffer (url-retrieve-synchronously url))
		(error (throw 'ret nil)))) ; Return nil if url cannot be retrieved
    t)) ; Return true if URLs were retrieved successfully

;;; Try to silence GPG errors on Windows.
(cppimmo:when-system 'windows
  (setq package-check-signature nil))

(defun cppimmo:add-to-package-archives (name link)
  (add-to-list 'package-archives '(name . link) t))

;;; Define and initialise package repositories.
;;; GNU and NONGNU in list by default.
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;;; Install use-package to simplify the configuration file.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (require 'use-package)
  (when (cppimmo:internet-connection-p)
    (setq use-package-always-ensure t))
  (setq use-package-expand-minimally t))
;;(use-package gnu-elpa-keyring-update)

;;; NECESSARY STARTUP PACKAGES ==================================================

;;; Namespace utility
(use-package names)

;;; BASIC STARTUP STUFF =========================================================
;;; Append my own Emacs Lisp library directories to the load-path variable.

(mapcar (lambda (path) ; Prefix PATH with user-emacs-directory.
		  (funcall #'cppimmo:append-to-load-path path t))
		(list "cppimmo" "addons"))

;;; Load library files.
(load "cppimmo-count-words-mode")
(load "cppimmo-delim-face-mode")
(load "cppimmo-cl-face")
(load "cppimmo-commands")
;;(cppimmo:load-directory "~/.emacs.d/cppimmo/")

;;;(defun cppimmo:cycle-custom-themes ()
;;;  "Cycle through the known custom themes."
;;;  (interactive)
;;;  (let (($known-themes)
;;;		($active-theme))
;;;	(setq $known-themes (cl-set-difference custom-known-themes
;;;										   '(use-package user changed))
;;;		  $active-theme (car custom-enabled-themes))
;;;	(disable-theme $active-theme)
;;;	(enable-theme (car $known-themes))))
;;;(run-with-idle-timer
;;; 10 nil
;;; (lambda ()
;;;   (let* (($time-list    (decode-time))
;;;		  ($time-hours   (nth 2 $time-list))
;;;		  ($time-minutes (nth 2 $time-minutes)))
;;;	 (cond (()
;;;			)))))

;;; User Iterface
(progn
  (defun cppimmo:configure-frame-size (width height)
	"Set the initial frame size for floating window managers.
@WIDTH the desired character width of the frame.
@HEIGHT the desired character height of the frame."
	(when window-system
	  (set-frame-size (selected-frame) width height)))
  
  (cppimmo:when-system 'windows
	(cppimmo:configure-frame-size 120 40)) ; Set frame size

  (require 'subr-x)
  (cppimmo:when-system 'windows
	(setq user-full-name (getenv "USERNAME")))
  (setq inhibit-startup-message nil
		initial-scratch-message (concat "Welcome, " (capitalize (user-login-name)) "!")
		;; Set the frame title format (using backtick which produces list and comma operator to eval)		
		frame-title-format `("GNU Emacs - %b | " ,(user-login-name) "@" ,(system-name))
		cursor-type 'box ; Set the cursor type
		font-lock-maximum-decoration t ; Max font decor
		confirm-kill-emacs 'y-or-n-p)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (add-to-list 'custom-theme-load-path "~/.emacs.d/cppimmo-themes/") ; Set theme load path.
  ;;(load-theme 'cppimmo-bright-ink t) ; Set the theme (if custom).
  (add-hook 'prog-mode-hook #'show-paren-mode) ; Extra highlighting for programming modes.

  ;; Enable various default modes.
  (global-font-lock-mode t) ; Ensure syntax highlighting is always enabled.  
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
  ) ; End of user interface settings.

;;; File backup & saving configuration
(progn
  (defun cppimmo:make-backup-file-name (file-path)
	"This function from Xah Lee creates new directories for backups.
It creates directories that do not exist in the backup root.
Other methods of backup can easily exceed the MAX_PATH of POSIX-esque systems."
	(let (backup-root backup-file-path) ; let* could be used here, but it would be ugly.
	  (setq backup-root "~/.emacs.d/backup/")
	  ;; This format remove the Windows drive letter.
	  (setq backup-file-path
			(format "%s%s~" backup-root
					(replace-regexp-in-string "^[A-Za-z]:/" "" file-path)))
	  (make-directory
	   (file-name-directory backup-file-path)
	   (file-name-directory backup-file-path))
	  backup-file-path)) ; Return backup-file-path string.
 
  (setq make-backup-files t ; Make sure backups are enabled.
		make-backup-file-name-function 'cppimmo:make-backup-file-name) ; Set the backup file name function

  ;; Preserve creation date on Windows (irrelevant on UNIX-like systems).
  (cppimmo:when-system 'windows
	(setq backup-by-copying t))
  ;; (setq create-lockfiles nil)

  ;; Remember the cursor position in previously visited files.
  (if (version< emacs-version "25.0")
	  (progn
		(require 'saveplace)
		(setq-default save-place t))
	(save-place-mode 1))
  
  (setq auto-save-default nil) ; I save impulsively, so disabling this is fine.

  ;; I'm unsure if I should use this option, due to my use of cloud syncing clients
  ;; such as MEGA.  The file may be modified by MEGA and I could lose my work.
  ;; Although, this may be a moot concern.  Nevertheless, I will leave it incase I
  ;; believe I can use it in the future.
  ;; (global-auto-revert-mode 1) ; Reload buffer upon file modification.
  ) 

;;; Miscellaneous
(progn
  ;; Confirmation input settings.
  (if (version< emacs-version "28.1")
	  (defalias 'yes-or-no-p 'y-or-n-p-)
	(setq use-short-answers t)) ; I don't want to type yes or no each time.

  (setq default-directory user-emacs-directory ; Set the default directory (for find-file, etc.)
		mouse-highlight t ; NOTE: The highlighting can white-out text on darker themes
		shift-select-mode t ; I want to have select with shift + movement keys
		line-move-visual t ; 
		set-mark-command-repeat-pop nil ; Disable mark popping (ex: Cu C-SPC)
		mark-ring-max 10 ; 10 yanks limit
		global-mark-ring-max 10 ; 10 yanks limit
		)

  (delete-selection-mode t) ; Enable overwriting of marked region.
  (global-superword-mode -1) ; Treat snake_case, etc. as a single word.
  
  ;; Enable disabled features.
  (progn
	(put 'upcase-region 'disabled nil) ; Upcase region C-x C-u
	(put 'downcase-region 'disabled nil) ; Downcase region C-x C-l
	) ; End of enabling disabled features.

  ;; Enable fido-vertical-mode (fido-mode enabled also)
  ;; Alternative is icomplete-mode
  (when (version< "28.1" emacs-version)
    (fido-vertical-mode 1)) ; Will search & show commands with term regardless of position.
  ) ; End of miscellaneous.

;;; PACKAGE CONFIGURATION - NON-REPOS ===========================================

;;; Install and configure glsl-mode.
(progn
  (autoload 'glsl-mode "glsl-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode)))

;;; PACKAGE CONFIGURATION - MY STUFF ============================================

(progn
  ;; Configure cppimmo:count-words-mode
  (progn
	(defun cppimmo:my-count-words-mode-hook ()
	  (setq *cppimmo:count-words-use-header-line* nil))
	;; Add custom hook
	(add-hook 'cppimmo:count-words-mode-hook #'cppimmo:my-count-words-mode-hook))
	
  ;; Enable delim face mode
  (cppimmo:global-delim-face-mode 1))

;;; PACKAGE CONFIGURATION - REPOS ===============================================

;;; Settings for the pomodoro package.
(use-package pomodoro
  :defer 25 ; Wait 25 secs to load the package due to doom-modeline being loaded after this declaration
  :config
  (progn
	;; Add to the mode line once
	(defvar cppimmo:pomodoro-mode-line-set-p nil)
	(unless cppimmo:pomodoro-mode-line-set-p
	  (pomodoro-add-to-mode-line)		; Add to modeline.
	  (setq cppimmo:pomodoro-mode-line-set-p t))
	;; Place all audio files in the repository to make things easier.
	(setq pomodoro-work-start-sound  "~/.emacs.d/audio/beacon-alarm.wav" ; Work time alert.
		  pomodoro-break-start-sound "~/.emacs.d/audio/beacon-alarm.wav") ; Break time alert.
	(defun cppimmo:play-pomodoro-sound (sound)
	  "Replace the play sound function for the pomodoro package."
	  (play-sound-file (expand-file-name sound)))
	;; Properly replace the play sound function.
	(advice-add 'play-pomodoro-sound :override #'cppimmo:play-pomodoro-sound)))

;;; Install and configure php-mode
(use-package php-mode)

;;; Install and configure lua-mode
(use-package lua-mode
  :config
  (progn
  (setq lua-indent-level 4)
  (setq lua-indent-string-contents t)
  (defun cppimmo:lua-mode-hook ()
	"I want tabs!"
	(setq indent-tabs-mode t) ; Enable indent tabs mode via the mode-hook.
	(abbrev-mode nil)) ; This probably isn't really relevant.
  (add-hook 'lua-mode-hook #'cppimmo:lua-mode-hook)))

;;; Install and configure ox-leanpub
(use-package ox-leanpub)

;;; Install and configure markdown-mode.
(use-package markdown-mode
  :config
  ;; The the appropriate "markdown-command" for Microsoft Windows.
  (cppimmo:when-system 'windows
    (custom-set-variables '(markdown-command "pandoc.exe"))))
;;; Install and configure markdown-preview-eww.
;;(use-package markdown-preview-eww)

;;; Install the 2048-game package.
(use-package 2048-game)

;;; Install the doom-themes packages and set the theme.
(use-package doom-themes
  :config
  (progn
    ;;(load-theme 'doom-1337 t))
    (load-theme 'doom-monokai-classic t)))

;;; Install and configure SLIME.
(use-package slime
  :config (setq inferior-lisp-program "sbcl"))

(use-package slime-company
  :after (slime company)
  :config (setq slime-company-completion 'fuzzy
				slime-company-after-completion 'slime-company-just-one-space))

(slime-setup '(slime-fancy slime-company slime-cl-indent slime-fuzzy slime-quicklisp slime-asdf))

;;; Install and configure magit.
(use-package magit)

;;; Install and configure highlight numbers.
(use-package highlight-numbers
  :config
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

;;; Install and configure highlight parentheses.
(use-package highlight-parentheses
  :config
  (add-hook 'prog-mode-hook #'highlight-parentheses-mode)
  (add-hook 'minibuffer-setup-hook #'highlight-parentheses-minibuffer-setup))

;;; Install and configure elfeed.
(use-package elfeed
  :config
  (progn
    (load "~/.emacs.d/cppimmo/cppimmo-feeds-pub.el")
    ;; Load private feed file if it exists and append items to list.
    (let ((feed-file-priv "~/.emacs.d/cppimmo/cppimmo-feeds-priv.el"))
      (when (file-exists-p feed-file-priv)
	(load feed-file-priv)))))

;;; Install and configure ement.
;;(use-package ement)

;;; Install and configure web-mode.
;;; https://web-mode.org/
(use-package web-mode
  :config
  ;; TODO: Style left padding.
  (defun cppimmo:web-mode-hook ()
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
  (add-hook 'web-mode-hook #'cppimmo:web-mode-hook)
  ;; Load all php files in web-mode by default
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode)))

;;; Install and confiure css-eldoc.
(use-package css-eldoc
  :config
  (add-hook 'css-mode-hook 'turn-on-css-eldoc)
  (add-hook 'scss-mode-hook 'turn-on-css-eldoc))

;;; Install and configure helpful.
(use-package helpful)
;; helpful-callabe, -function, -macro, -command, -key, -variable, -at-point

;;; Install and configure auctex.
;;(use-package auctex)

;;; Install and configure powershell.
(use-package powershell)

;;; Install and configure olivetti.
(use-package olivetti)

;;; Install and configure pdf-tools.
;;(use-package pdf-tools)

;;; Install and configure cursory.
(use-package cursory
  :config (cursory-set-preset
		   (or (cursory-restore-latest-preset) 'box)))

;;; Install and configure cmake-mode.
(use-package cmake-mode)
;;; Install and configure cmake-font-lock.
(use-package cmake-font-lock)
;;; Install and configure cmake-ide.
(use-package cmake-ide)
;;; Install and configure cmake-project.
(use-package cmake-project)

;;; Install and configure org-journal.
(use-package org-journal
  :config (setq org-journal-dir "~/.emacs.d/org-journal"))

;;; Install and configure ement.
(use-package ement)

;;; Install and configure geiser.
(use-package geiser
  :config
  (cppimmo:when-system 'linux
	(setq geise-active-implementations '(mit racket))))
(use-package geiser-racket)
(use-package geiser-mit
  :config
  (cppimmo:when-system 'linux ; Prefer mit-scheme on GNU/Linux.
	(setq geiser-mit-binary "/usr/bin/scheme")))

;;; Install and configure racket-mode.
(use-package racket-mode)

;;; Install and configure company.
(use-package company
  :config
  (setq company-idle-delay 0.15 ; Delay in seconds.
		company-minimum-prefix-length 3 ; Three characters minimum.
		company-selection-wrap-around t) ; Wrap compeletion dropdown navigation.
  (add-hook 'prog-mode-hook 'company-mode))

;;; Install and configure nyan-mode.
(use-package nyan-mode
  :config
  ;;(cppimmo:when-system 'linux
    ;;(nyan-mode 1))
  )

;;; Install and configure info-colors.
(use-package info-colors
  :config
  (add-hook 'Info-selection-hook 'info-colors-fontify-node))

;;; Install and configure paren-face.
(use-package paren-face)

;;; Install and configure winum.
;;; Select window <n> | C-x w <n>.
;;; Select window greater than 10 | C-x w `.
(use-package winum
  :config
  (setq winum-scope 'frame-local
		winum-format "[%s]")
  (winum-mode))

;;; Install and configure origami.
(use-package origami
  :config
  (add-hook 'prog-mode-hook #'global-origami-mode)
  (define-key origami-mode-map (kbd "M-o") #'origami-recursively-toggle-node))

;;; Install and configure naysayer.
(use-package naysayer-theme)

;;; Use M-x nerd-icons-install-fonts to setup fonts
(use-package nerd-icons
  :custom (nerd-icons-font-family "Symbols Nerd Font Mono"))

;;; Display nerd-icons within dired mode
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

;;; Customized modeline used in Doom Emacs
(use-package doom-modeline
  :config (doom-modeline-mode 1)
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  ;;(doom-modeline-enable-word-count t)
  (doom-modeline-buffer-encoding t)
  (doom-modeline-buffer-modification-icon t)
  ;; Needs display-time-mode to be on
  (doom-modeline-time t))

;;; Autocomplete for C/C++ stdlib headers
(use-package company-c-headers
  :config (add-to-list 'company-backends 'company-c-headers))

;;; vtermlib for LINUX
(cppimmo:when-system '(linux macos bsd)
  (use-package vterm
	:config
	(progn
	  (defun cppimmo:pathname-contains-home-p (path)
		"Predicate check if PATH is proceeded by the operating...
system's home directory."
		(string-match (concat
					   ;; Eval args until one of the yields non-nil, then return that value.
					   (or (cppimmo:when-system 'linux "/home/")
						   ;; Actually don't need windows lol
						   (cppimmo:when-system 'windows "C:/Users/"))
					   (user-login-name))
					  default-directory))
	  (defun cppimmo:pathname-prettify-home (path)
		(let ((start-idx (cppimmo:pathname-contains-home-p path))
			  (end-idx (match-end 0)))
		  (if (not (null start-idx))
			  (concat "~/" (substring path (+ end-idx 1)))
			path)))
	  ;; This avoids doing any shell-side configuration.
	  (defun cppimmo:vterm ()
		"Command to launch a new vterm buffer.
The buffer will be renamed automatically with the buffer local directory."
		(interactive)
		(with-current-buffer (vterm) ; vterm returns the buffer
		  (rename-buffer
		   (concat "*" (user-login-name) "@" (system-name) ":"
				   ;; Append 
				   (let ((result (cppimmo:pathname-prettify-home default-directory)))
					 (if (string= (substring result -1) "/") ; Negative substring idx's count backwards
						 result
					   (concat result "/")))
				   "*"))
		  (rename-uniquely))))
	 (bind-key* (kbd "C-c C-v") #'cppimmo:vterm)))

;;; Interactive REPL for PHP
(use-package psysh)

(use-package php-eldoc)

;;; Major mode for viewing logs
(use-package logview)

;;; Configure emoji display
(use-package emojify
  :config
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
      ((member "Symbola" (font-family-list)) "Symbola")))
  (setq emojify-display-style 'unicode
		emojify-emoji-styles '(unicode))
  ;; Binding for inserting unicode emoji (binding similar to insert-char):
  (bind-key* (kbd "C-x 8 C-<return>") #'emojify-insert-emoji))

;;; Major mode for GDScript used by the Godot game engine
(use-package gdscript-mode
  :config
  (progn
	(setq gdscript-use-tab-indents t
		  gdscript-indent-offset 4
		  gdscript-gdformat-save-and-format t)
	(cppimmo:when-system 'windows
	  (setq gdscript-godot-executable "godot.exe"))))

;;; Circe IRC
(use-package circe
  :config
  (progn
	;; Create .authinfo file and enter the following:
	;; machine irc.libera.chat login yournick password password port 6667
	;; To encrypt enter the following command:
	;; gpg -e r myemail@email.com .authinfo
	(setq auth-sources '("~/.emacs.d/.authinfo.gpg"))
	
	(defun cppimmo:circe-fetch-password (&rest params)
	  (require 'auth-source)
	  (let ((match (car (apply 'auth-source-search params))))
		(if match
			(let ((secret (plist-get match :secret)))
			  (if (functionp secret)
				  (funcall secret)
				secret))
		  (error "Password not found for %S" params))))
	
	(defun cppimmo:circe-nickserv-password (server)
	  (cppimmo:circe-fetch-password :max 1 :user "cppimmo" :host "irc.libera.chat" :port 6667))
	
	(setq lui-logging-directory "~/.emacs.d/.lui-logs/"
		  circe-format-self-say "<{nick}> {body}"
		  circe-network-options
		  `(("Libera Chat"
			 :logging t
			 :nick "cppimmo"
			 :channels (:after-auth "##slackware"
									"##slackware-help"
									"##slackware-offtopic"
									"#slackbuilds"
									"#msb"
									"#sbopkg"
									"#slackdocs"
									"#freebsd-irc"
									"#emacs")
			 :nickserv-password cppimmo:circe-nickserv-password))))

  (add-hook 'circe-chat-mode-hook #'cppimmo:circe-prompt)
  (defun cppimmo:circe-prompt ()
	(lui-set-prompt
	 (concat (propertize (concat (buffer-name) ">")
						 'face 'circe-prompt-face)
			 " ")))

  (defun cppimmo:irc-network-connected-p (network)
	"Return non-nil if there's any Circe server-buffer whose
`circe-server-netwok' is NETWORK."
	(catch 'return
      (dolist (buffer (circe-server-buffers))
		(with-current-buffer buffer
          (if (string= network circe-server-network)
              (throw 'return t))))))

  (defun cppimmo:irc-connect (network)
	"Connect to NETWORK, but ask user for confirmation if it's
already been connected to."
	(interactive "sNetwork: ")
	(if (or (not (circe-network-connected-p network))
			(y-or-n-p (format "Already connected to %s, reconnect?" network)))
		(circe network))))

;; Configure clojure-mode
(use-package clojure-mode)

;; Configure cider
(use-package cider
  :defer t
  :init (progn
		  (add-hook 'clojure-mode-hook 'cider-mode)
		  (add-hook 'clojurescript-mode-hook 'cider-mode)
		  (add-hook 'clojurec-mode-hook 'cider-mode)
		  (add-hook 'cider-repl-mode-hook 'cider-mode))
  :config
  (setq cider-repl-display-help-banner nil
		cider-auto-mode nil))

;; Configure paredit
(use-package paredit
  :config (progn
			;; Set hooks for various lisp modes
			(mapc (lambda (mode-hook)
					(when (boundp mode-hook) ; Ensure symbol in MODE-HOOK exists
					  (add-hook mode-hook 'enable-paredit-mode)))
				  '(emacs-lisp-mode-hook	   ; Core Emacs lisp mode
					lisp-mode-hook			   ; Core lisp mode
					lisp-interaction-mode-hook ; Core lisp mode
					scheme-mode-hook
					racket-mode-hook
					clojure-mode-hook
					clojurescript-mode-hook
					cider-repl-mode-hook
					slime-repl-mode-hook))))

;;; Visually distinguish file attached buffers from others such as REPL
(use-package solaire-mode
  :hook (after-init . solaire-global-mode))

;;; Expand/collapse regions.
;;;(use-package expand-region
;;;  :general ("C-q" 'er/expand-region))

;;; BUILT-IN MODE CONFIGURATION =================================================

;;; Emacs bultin package configuration
(use-package emacs
  :config
  ;; Set the fill column in auto fill mode.
  (add-hook 'text-mode-hook
			(lambda ()
			  ;; (turn-on-auto-fill) ; Keep as reference.
			  (set-fill-columnface mode 80))))

(use-package ispell
  :ensure nil
  :config
  ;; Configuration for the ispell functionality on Windows.
  ;; The trick is to use msys2 and the MinGW hunspell and hunspell-en packages.
  ;; https://www.reddit.com/r/emacs/comments/8by3az/how_to_set_up_sell_check_for_emacs_in_windows/
  ;; https://stackoverflow.com/questions/8931580/hunspell-cant-open-affix-or-dictionary-files-for-dictionary-named-en-us
  (cppimmo:when-system 'windows
    (setq ispell-program-name "C:/tools/msys64/mingw64/bin/hunspell.exe" ; Set the executable pathname.
		  ispell-dictionary "en_US"))) ; Set the appropriate word dictionary.
  
(use-package cc-mode
  :ensure nil
  :config
  ;; Configuration for the CC mode.
  (setq c-default-style "bsd")
  (setq-default c-basic-offset 4
				tab-width 4
				indent-tabs-mode t))

(use-package dired
  :ensure nil
  :config
  ;; Dired.
  ;; From: https://www.emacswiki.org/emacs/DiredSortDirectoriesFirst
  (defun cppimmo:dired-sort ()
	"Make dired listings sort and display directories first."
	(save-excursion
	  (let (buffer-read-only)
		(forward-line 2) ; Move beyond directory heading.
		(sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
	  (set-buffer-modified-p nil)))
  
  (defadvice dired-readin
	  (after dired-after-updating-hook first () activate)
	"Make dired sort listings with directories first before adding marks."
	(cppimmo:dired-sort))
  
  (defun cppimmo:dired-open-in-new-frame ()
	"Open a file/directory in a new frame."
	(interactive)
	(find-file-other-frame (dired-get-file-for-visit))))

(use-package abbrev
  :ensure nil
  :config
  ;; Load abbreviations (abbrev-mode).
  (load "~/.emacs.d/cppimmo/cppimmo-abbrev.el"))

(use-package bookmark
  :ensure nil
  :config
  ;; Bookmarks
  (setq bookmark-save-flag 1 ; Save bookmark file after one modification.
		;; Ensure bookmark file default doesn't change.
		bookmark-default-file "~/.emacs.d/bookmarks"))

(use-package nxml-mode
  :ensure nil
  :config
  (defun cppimmo:xml-insert-cdata ()
	"Insert CDATA tags for XML documents.
Moves the point back 3 characters for immediate editing."
	(interactive)
	(let ((left-side "<![CDATA[") (right-side "]]>"))
	  (insert left-side right-side)
	  (backward-char (length right-side)))))

;;; css-mode
(use-package css-mode
  :ensure nil
  :config
  (defun cppimmo:css-mode-hook ()
	"My css-mode hook."
	(setq-local css-indent-offset 2))
  (add-hook 'css-mode-hook #'cppimmo:css-mode-hook))

;;; rst-mode
(use-package rst
  :ensure nil
  :config
  (defun cppimmo:rst-mode-hook ()
	"My rst-mode hook."
	(setq-local fill-column 80)
	(auto-fill-mode t))
  (add-hook 'rst-mode-hook #'cppimmo:rst-mode-hook))

;;; Diary
;;(use-package calender
;;  :config
;;  (setq european-calendar-style nil))

;;; sql-mode
;;(use-package sql
;;  :ensure nil
;;  :config
;;  (setq sql-product 'mysql)) ; Default SQL interpreter.

;;; Shell script mode
(use-package sh-script
  :config
  (progn
	(setq sh-indentation 2)))

(use-package bookmark
  :config
  ;; Set the initial startup buffer
  (defun cppimmo:retrieve-initial-buffer ()
	"Callback returns the buffer object which should be displayed
when Emacs starts."
	(list-bookmarks)
	(get-buffer bookmark-bmenu-buffer))
  ;; Assign the callback to initial-buffer-choice
  (setq initial-buffer-choice #'cppimmo:retrieve-initial-buffer))

(use-package autorevert
  ;; Ensure real-time file modifications are propogated in the buffer
  :config (global-auto-revert-mode 1))

;;; Settings for fill column indicator package. Toggle with "fci-mode".
(use-package fill-column-indicator
  :config
  (setq fci-rule-column 80
		fci-rule-width  2
		fci-rule-color  "red"))

;;; LOAD KEYBINDINGS ============================================================
(load "cppimmo-keybindings")
(cppimmo:bind-keys-g)
(cppimmo:bind-keys-m)

