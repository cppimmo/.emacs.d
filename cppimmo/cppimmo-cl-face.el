;;; BSD 2-Clause License
;;; 
;;; Copyright (c) 2023, Brian Hoffpauir
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
;;; Common Lisp Font Face Adjustments
;;;
;;; Implementation of this article: https://www.n16f.net/blog/custom-font-lock-configuration-in-emacs/
;;; Many thanks to the author.

(defface cppimmo/cl-character-face
  '((default :inherit font-lock-constant-face))
  "The face used to highlight Common Lisp character literals.")

(defface cppimmo/cl-standard-function-face
  '((default :inherit font-lock-keyword-face))
  "The face used to highlight standard Common Lisp function symbols.")

(defface cppimmo/cl-standard-value-face
  '((default :inherit font-lock-variable-name-face))
  "The face used to highlight standard Common Lisp value symbols.")

(defvar cppimmo/cl-function-names
  (list "*" "+" "-" "/" "/=" "1+" "1-" "<" "<=" "=" ">" ">=" "abort" "abs" "acons"
      "acos" "acosh" "add-method" "adjoin" "adjust-array" "adjustable-array-p"
      "allocate-instance" "alpha-char-p" "alphanumericp" "and" "append" "apply"
      "apropos" "apropos-list" "aref" "arithmetic-error-operands"
      "arithmetic-error-operation" "array-dimension" "array-dimensions"
      "array-displacement" "array-element-type" "array-has-fill-pointer-p"
      "array-in-bounds-p" "array-rank" "array-row-major-index" "array-total-size"
      "arrayp" "ash" "asin" "asinh" "assert" "assoc" "assoc-if" "assoc-if-not"
      "atan" "atanh" "atom" "bit" "bit-and" "bit-andc1" "bit-andc2" "bit-eqv"
      "bit-ior" "bit-nand" "bit-nor" "bit-not" "bit-orc1" "bit-orc2"
      "bit-vector-p" "bit-xor" "block" "boole" "both-case-p" "boundp" "break"
      "broadcast-stream-streams" "butlast" "byte" "byte-position" "byte-size"
      "caaaar" "caaadr" "caaar" "caadar" "caaddr" "caadr" "caar" "cadaar"
      "cadadr" "cadar" "caddar" "cadddr" "caddr" "cadr" "call-method" "car"
      "case" "catch" "ccase" "cdaaar" "cdaadr" "cdaar" "cdadar" "cdaddr" "cdadr"
      "cdar" "cddaar" "cddadr" "cddar" "cdddar" "cddddr" "cdddr" "cddr" "cdr"
      "ceiling" "cell-error-name" "cerror" "change-class" "char" "char-code"
      "char-downcase" "char-equal" "char-greaterp" "char-int" "char-lessp"
      "char-name" "char-not-equal" "char-not-greaterp" "char-not-lessp"
      "char-upcase" "char/=" "char<" "char<=" "char=" "char>" "char>="
      "character" "characterp" "check-type" "cis" "class-name" "class-of"
      "clear-input" "clear-output" "close" "clrhash" "code-char" "coerce"
      "compile" "compile-file" "compile-file-pathname" "compiled-function-p"
      "compiler-macro-function" "complement" "complex" "complexp"
      "compute-applicable-methods" "compute-restarts" "concatenate"
      "concatenated-stream-streams" "cond" "conjugate" "cons" "consp"
      "constantly" "constantp" "continue" "copy-alist" "copy-list"
      "copy-pprint-dispatch" "copy-readtable" "copy-seq" "copy-structure"
      "copy-symbol" "copy-tree" "cos" "cosh" "count" "count-if" "count-if-not"
      "ctypecase" "decf" "declaim" "decode-float" "decode-universal-time"
      "defclass" "defconstant" "defgeneric" "define-compiler-macro"
      "define-condition" "define-method-combination" "define-modify-macro"
      "define-setf-expander" "define-symbol-macro" "defmacro" "defmethod"
      "defpackage" "defparameter" "defsetf" "defstruct" "deftype" "defun"
      "defvar" "delete" "delete-duplicates" "delete-file" "delete-if"
      "delete-if-not" "delete-package" "denominator" "deposit-field" "describe"
      "describe-object" "destructuring-bind" "digit-char" "digit-char-p"
      "directory" "directory-namestring" "disassemble" "do" "do*"
      "do-all-symbols" "do-external-symbols" "do-symbols" "documentation"
      "dolist" "dotimes" "dpb" "dribble" "ecase" "echo-stream-input-stream"
      "echo-stream-output-stream" "ed" "eighth" "elt" "encode-universal-time"
      "endp" "enough-namestring" "ensure-directories-exist"
      "ensure-generic-function" "eq" "eql" "equal" "equalp" "error" "etypecase"
      "eval" "eval-when" "evenp" "every" "exp" "export" "expt" "fboundp"
      "fceiling" "fdefinition" "ffloor" "fifth" "file-author"
      "file-error-pathname" "file-length" "file-namestring" "file-position"
      "file-string-length" "file-write-date" "fill" "fill-pointer" "find"
      "find-all-symbols" "find-class" "find-if" "find-if-not" "find-method"
      "find-package" "find-restart" "find-symbol" "finish-output" "first" "flet"
      "float" "float-digits" "float-precision" "float-radix" "float-sign"
      "floatp" "floor" "fmakunbound" "force-output" "format" "formatter" "fourth"
      "fresh-line" "fround" "ftruncate" "funcall" "function" "function-keywords"
      "function-lambda-expression" "functionp" "gcd" "gensym" "gentemp" "get"
      "get-decoded-time" "get-dispatch-macro-character" "get-internal-real-time"
      "get-internal-run-time" "get-macro-character" "get-output-stream-string"
      "get-properties" "get-setf-expansion" "get-universal-time" "getf" "gethash"
      "go" "graphic-char-p" "handler-bind" "handler-case" "hash-table-count"
      "hash-table-p" "hash-table-rehash-size" "hash-table-rehash-threshold"
      "hash-table-size" "hash-table-test" "host-namestring" "identity" "if"
      "ignore-errors" "imagpart" "import" "in-package" "incf"
      "initialize-instance" "input-stream-p" "inspect" "integer-decode-float"
      "integer-length" "integerp" "interactive-stream-p" "intern" "intersection"
      "invalid-method-error" "invoke-debugger" "invoke-restart"
      "invoke-restart-interactively" "isqrt" "keywordp" "labels" "lambda" "last"
      "lcm" "ldb" "ldb-test" "ldiff" "length" "let" "let*"
      "lisp-implementation-type" "lisp-implementation-version" "list" "list*"
      "list-all-packages" "list-length" "listen" "listp" "load"
      "load-logical-pathname-translations" "load-time-value" "locally" "log"
      "logand" "logandc1" "logandc2" "logbitp" "logcount" "logeqv"
      "logical-pathname" "logical-pathname-translations" "logior" "lognand"
      "lognor" "lognot" "logorc1" "logorc2" "logtest" "logxor" "long-site-name"
      "loop" "loop-finish" "lower-case-p" "machine-instance" "machine-type"
      "machine-version" "macro-function" "macroexpand" "macroexpand-1" "macrolet"
      "make-array" "make-broadcast-stream" "make-concatenated-stream"
      "make-condition" "make-dispatch-macro-character" "make-echo-stream"
      "make-hash-table" "make-instance" "make-instances-obsolete" "make-list"
      "make-load-form" "make-load-form-saving-slots" "make-package"
      "make-pathname" "make-random-state" "make-sequence" "make-string"
      "make-string-input-stream" "make-string-output-stream" "make-symbol"
      "make-synonym-stream" "make-two-way-stream" "makunbound" "map" "map-into"
      "mapc" "mapcan" "mapcar" "mapcon" "maphash" "mapl" "maplist" "mask-field"
      "max" "member" "member-if" "member-if-not" "merge" "merge-pathnames"
      "method-combination-error" "method-qualifiers" "min" "minusp" "mismatch"
      "mod" "muffle-warning" "multiple-value-bind" "multiple-value-call"
      "multiple-value-list" "multiple-value-prog1" "multiple-value-setq"
      "name-char" "namestring" "nbutlast" "nconc" "nintersection" "ninth"
      "no-applicable-method" "no-next-method" "not" "notany" "notevery" "nreconc"
      "nreverse" "nset-difference" "nset-exclusive-or" "nstring-capitalize"
      "nstring-downcase" "nstring-upcase" "nsublis" "nsubst" "nsubst-if"
      "nsubst-if-not" "nsubstitute" "nsubstitute-if" "nsubstitute-if-not" "nth"
      "nth-value" "nthcdr" "null" "numberp" "numerator" "nunion" "oddp" "open"
      "open-stream-p" "or" "output-stream-p" "package-error-package"
      "package-name" "package-nicknames" "package-shadowing-symbols"
      "package-use-list" "package-used-by-list" "packagep" "pairlis"
      "parse-integer" "parse-namestring" "pathname" "pathname-device"
      "pathname-directory" "pathname-host" "pathname-match-p" "pathname-name"
      "pathname-type" "pathname-version" "pathnamep" "peek-char" "phase" "plusp"
      "pop" "position" "position-if" "position-if-not" "pprint" "pprint-dispatch"
      "pprint-exit-if-list-exhausted" "pprint-fill" "pprint-indent"
      "pprint-linear" "pprint-logical-block" "pprint-newline" "pprint-pop"
      "pprint-tab" "pprint-tabular" "prin1" "prin1-to-string" "princ"
      "princ-to-string" "print" "print-not-readable-object" "print-object"
      "print-unreadable-object" "probe-file" "proclaim" "prog" "prog*" "prog1"
      "prog2" "progn" "progv" "provide" "psetf" "psetq" "push" "pushnew" "quote"
      "random" "random-state-p" "rassoc" "rassoc-if" "rassoc-if-not" "rational"
      "rationalize" "rationalp" "read" "read-byte" "read-char"
      "read-char-no-hang" "read-delimited-list" "read-from-string" "read-line"
      "read-preserving-whitespace" "read-sequence" "readtable-case" "readtablep"
      "realp" "realpart" "reduce" "reinitialize-instance" "rem" "remf" "remhash"
      "remove" "remove-duplicates" "remove-if" "remove-if-not" "remove-method"
      "remprop" "rename-file" "rename-package" "replace" "require" "rest"
      "restart-bind" "restart-case" "restart-name" "return" "return-from"
      "revappend" "reverse" "room" "rotatef" "round" "row-major-aref" "rplaca"
      "rplacd" "sbit" "scale-float" "schar" "search" "second" "set"
      "set-difference" "set-dispatch-macro-character" "set-exclusive-or"
      "set-macro-character" "set-pprint-dispatch" "set-syntax-from-char" "setf"
      "setq" "seventh" "shadow" "shadowing-import" "shared-initialize" "shiftf"
      "short-site-name" "signal" "signum" "simple-bit-vector-p"
      "simple-condition-format-arguments" "simple-condition-format-control"
      "simple-string-p" "simple-vector-p" "sin" "sinh" "sixth" "sleep"
      "slot-boundp" "slot-exists-p" "slot-makunbound" "slot-missing"
      "slot-unbound" "slot-value" "software-type" "software-version" "some"
      "sort" "special-operator-p" "sqrt" "stable-sort" "standard-char-p" "step"
      "store-value" "stream-element-type" "stream-error-stream"
      "stream-external-format" "streamp" "string" "string-capitalize"
      "string-downcase" "string-equal" "string-greaterp" "string-left-trim"
      "string-lessp" "string-not-equal" "string-not-greaterp" "string-not-lessp"
      "string-right-trim" "string-trim" "string-upcase" "string/=" "string<"
      "string<=" "string=" "string>" "string>=" "stringp" "sublis" "subseq"
      "subsetp" "subst" "subst-if" "subst-if-not" "substitute" "substitute-if"
      "substitute-if-not" "subtypep" "svref" "sxhash" "symbol-function"
      "symbol-macrolet" "symbol-name" "symbol-package" "symbol-plist"
      "symbol-value" "symbolp" "synonym-stream-symbol" "tagbody" "tailp" "tan"
      "tanh" "tenth" "terpri" "the" "third" "throw" "time" "trace"
      "translate-logical-pathname" "translate-pathname" "tree-equal" "truename"
      "truncate" "two-way-stream-input-stream" "two-way-stream-output-stream"
      "type-error-datum" "type-error-expected-type" "type-of" "typecase" "typep"
      "unbound-slot-instance" "unexport" "unintern" "union" "unless"
      "unread-char" "untrace" "unuse-package" "unwind-protect"
      "update-instance-for-different-class" "update-instance-for-redefined-class"
      "upgraded-array-element-type" "upgraded-complex-part-type" "upper-case-p"
      "use-package" "use-value" "user-homedir-pathname" "values" "values-list"
      "vector" "vector-pop" "vector-push" "vector-push-extend" "vectorp" "warn"
      "when" "wild-pathname-p" "with-accessors" "with-compilation-unit"
      "with-condition-restarts" "with-hash-table-iterator"
      "with-input-from-string" "with-open-file" "with-open-stream"
      "with-output-to-string" "with-package-iterator" "with-simple-restart"
      "with-slots" "with-standard-io-syntax" "write" "write-byte" "write-char"
      "write-line" "write-sequence" "write-string" "write-to-string" "y-or-n-p"
      "yes-or-no-p" "zerop")
  "Common Lisp function name symbol list.")

(defvar cppimmo/cl-value-names
  (list "*" "**" "***" "*break-on-signals*" "*compile-file-pathname*"
		"*compile-file-truename*" "*compile-print*" "*compile-verbose*"
		"*debug-io*" "*debugger-hook*" "*default-pathname-defaults*"
		"*error-output*" "*features*" "*gensym-counter*" "*load-pathname*"
		"*load-print*" "*load-truename*" "*load-verbose*" "*macroexpand-hook*"
		"*modules*" "*package*" "*print-array*" "*print-base*" "*print-case*"
		"*print-circle*" "*print-escape*" "*print-gensym*" "*print-length*"
		"*print-level*" "*print-lines*" "*print-miser-width*"
		"*print-pprint-dispatch*" "*print-pretty*" "*print-radix*"
		"*print-readably*" "*print-right-margin*" "*query-io*" "*random-state*"
		"*read-base*" "*read-default-float-format*" "*read-eval*" "*read-suppress*"
		"*readtable*" "*standard-input*" "*standard-output*" "*terminal-io*"
		"*trace-output*" "+" "++" "+++" "-" "/" "//" "///" "array-dimension-limit"
		"array-rank-limit" "array-total-size-limit" "boole-1" "boole-2" "boole-and"
		"boole-andc1" "boole-andc2" "boole-c1" "boole-c2" "boole-clr" "boole-eqv"
		"boole-ior" "boole-nand" "boole-nor" "boole-orc1" "boole-orc2" "boole-set"
		"boole-xor" "call-arguments-limit" "char-code-limit" "double-float-epsilon"
		"double-float-negative-epsilon" "internal-time-units-per-second"
		"lambda-list-keywords" "lambda-parameters-limit"
		"least-negative-double-float" "least-negative-long-float"
		"least-negative-normalized-double-float"
		"least-negative-normalized-long-float"
		"least-negative-normalized-short-float"
		"least-negative-normalized-single-float" "least-negative-short-float"
		"least-negative-single-float" "least-positive-double-float"
		"least-positive-long-float" "least-positive-normalized-double-float"
		"least-positive-normalized-long-float"
		"least-positive-normalized-short-float"
		"least-positive-normalized-single-float" "least-positive-short-float"
		"least-positive-single-float" "long-float-epsilon"
		"long-float-negative-epsilon" "most-negative-double-float"
		"most-negative-fixnum" "most-negative-long-float"
		"most-negative-short-float" "most-negative-single-float"
		"most-positive-double-float" "most-positive-fixnum"
		"most-positive-long-float" "most-positive-short-float"
		"most-positive-single-float" "multiple-values-limit" "nil" "pi"
		"short-float-epsilon" "short-float-negative-epsilon" "single-float-epsilon"
		"single-float-negative-epsilon" "t")
  "Common Lisp variable names symbol list.")

(defvar cppimmo/cl-font-lock-keywords
  (let* ((character-regex (concat "#\\\\" lisp-mode-symbol-regexp "\\_>"))
		 (function-regex (concat "(" (regexp-opt cppimmo/cl-function-names t) "\\_>"))
		 (value-regex (regexp-opt cppimmo/cl-value-names 'symbols)))
	`((,character-regex . 'cppimmo/cl-character-face)
	  (,function-regex
	   (1 'cppimmo/cl-standard-function-face))
	  (,value-regex . 'cppimmo/cl-standard-value-face))))

(defvar cppimmo/cl-font-lock-defaults
  '((cppimmo/cl-font-lock-keywords)
	nil ; Enable syntax highlighting
	t   ; Case insensitive highlighting
	nil ; Use the lisp-mode syntax table
	(font-lock-mark-block-function . mark-defun)
	(font-lock-extra-managed-props help-echo)
	(font-lock-syntactic-face-function . lisp-font-lock-syntactic-face-function)))

(defun cppimmo/init-lisp-font-lock ()
  (setq font-lock-defaults cppimmo/cl-font-lock-defaults))

(add-hook 'lisp-mode-hook 'cppimmo/init-lisp-font-lock)
