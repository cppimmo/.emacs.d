;;; -*- coding: utf-8; lexical-binding: t; -*-
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
;;; Abbreviation Definitions.
;;;
;;(setq save-abbrev nil)
(clear-abbrev-table global-abbrev-table)
(define-abbrev-table 'global-abbrev-table
  '(
	;; Net lingo.
	("ur" "you are")
	("afaik" "as far as I know")
	("ty" "thank you")
	("btw" "by the way")
	("atm" "at the moment")
	("brb" "be right back")
	("bc" "because")
	("iirc" "if I recall correctly")
	("hbu" "how about you")
	("lol" "laugh out loud")
	("gn" "good night")
	("gm" "good morning")
	;; Prog lang.
	))

;;; Prefix with major mode function name.
(when (boundp 'c++-mode-abbrev-table)
  (clear-abbrev-table c++-mode-abbrev-table))

(define-abbrev-table 'c++-mode-abbrev-table
  '(
	("std" "std::")
	))

(set-default 'abbrev-mode t) ; Enable abbrev-mode in most contexts.
