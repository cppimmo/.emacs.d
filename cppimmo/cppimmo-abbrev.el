;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Abbreviation Definitions.
;;

;; (setq save-abbrev nil)
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

;; Prefix with major mode function name.
(when (boundp 'c++-mode-abbrev-table)
  (clear-abbrev-table c++-mode-abbrev-table))

(define-abbrev-table 'c++-mode-abbrev-table
  '(
	("std" "std::")
	))

(set-default 'abbrev-mode t) ; Enable abbrev-mode in most contexts.
