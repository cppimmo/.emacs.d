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
(defun cppimmo/xml-time-string ()
  "Return formatted time string."
  (format-time-string "%m/%d/%y"))

(defun cppimmo/xml-insert-cdata ()
  "Insert CDATA tags for XML documents.
Moves the point back 3 characters for immediate editing."
  (interactive)
  (insert "<![CDATA[]]>")
  (backward-char 3))

(defun cppimmo/xml-insert-blog ()
  "Insert XML tags that I use for my website's blog."
  (interactive)
  (insert "<?xml version=\"1.0\"?>\n")
  (insert "<post>\n")
  (insert "  <title></title>\n")
  (insert "  <link></link>\n")
  (insert "  <description></description>\n")
  (insert (concat "  <pubDate>" (cppimmo/xml-time-string) "</pubDate>\n"))
  (insert "  <readTime></readTime>\n")
  (insert "</post>\n"))

(defun cppimmo/xml-insert-blog-rss-item ()
  "Insert XML blog RSS feed entry item.
Use for inserting relevant tags into the <channel> tag of an existing XML
document."
  (interactive)
  (insert "<item>\n")
  (insert "  <title></title>\n")
  (insert "  <link></link>\n")
  (insert "  <filename>.xml</filename>\n")
  (insert "  <description></description>\n")
  (insert (concat "  <pubDate>" (cppimmo/xml-time-string) "</pubDate>\n"))
  (insert "  <readTime></readTime>\n")
  (insert "</item>"))
