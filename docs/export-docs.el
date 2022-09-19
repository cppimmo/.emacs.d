#!/usr/bin/emacs --script
;;; Use this script to evalute an org-mode buffer and export to a markdown file.

(defun cppimmo-export-to-markdown (@file-name-in @file-name-out)
  "Export an org-mode file to a markdown file.
@FILE-NAME-IN input file name.
@FILE-NAME-OUT output file name."
  (find-file @file-name-in)
  (org-md-export-as-markdown)
  (write-file @file-name-out))
;; Export the README.org and place in the repository root.
(cppimmo-export-to-markdown "~/.emacs.d/docs/README.org"
							"~/.emacs.d/README.md")
;; Export the TODO.org and place in the repository root.
(cppimmo-export-to-markdown "~/.emacs.d/docs/TODO.org"
							"~/.emacs.d/TODO.md")
