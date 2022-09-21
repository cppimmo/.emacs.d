#!/usr/bin/emacs --script
;;; Byte compile the elisp files in the cppimmo/ directory.
;;

;; TODO: Generate the list by searching for .el files in the directory.
(setq file-list (list "cppimmo-count-words-mode.el"
					  "cppimmo-dvorak.el"
					  "cppimmo-xml.el"))
(setq file-parent-directory "~/.emacs.d/cppimmo/")

(defun cppimmo/byte-compile (@file-name)
  ""
  (if (eq (byte-compile-file (concat file-parent-directory @file-name)) nil)
	  (message "%s compilation failiure!" @file-name)
	(message "%s compiled successfully." @file-nameq)))
  
(mapc #'cppimmo/byte-compile file-list)
