#!/usr/bin/emacs --script
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
;;; Byte compile the elisp files in the cppimmo/ directory.
;;;
;;; TODO: Generate the list by searching for .el files in the directory.

(defun cppimmo/byte-compile (@file-parent-dir @file-name)
  "Byte compile @FILE-NAME."
  (if (eq (byte-compile-file (concat @file-parent-dir @file-name)) nil)
	  (message "%s compilation failiure!" @file-name)
	(message "%s compiled successfully." @file-name)))

(let (($file-list)
	  ($file-parent-dir))
  (setq $file-list (list "cppimmo-abbrev.el"
						 "cppimmo-commands.el"
						 "cppimmo-count-words-mode.el"
						 "cppimmo-delim-face-mode.el"
						 "cppimmo-dvorak.el"
						 "cppimmo-keybindings.el"
						 "cppimmo-xml.el")
		$file-parent-dir "~/.emacs.d/cppimmo/")
  (mapc (lambda (@file-name)
		  (funcall #'cppimmo/byte-compile $file-parent-dir @file-name)
		  $file-list)))
