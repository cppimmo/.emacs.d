
(defun cppimmo-xml-time-string ()
  "Return formatted time string."
  (format-time-string "%m/%d/%y"))

(defun cppimmo-insert-xml-cdata ()
  "Insert CDATA tags for XML documents.
Moves the point back 3 characters for immediate editing."
  (interactive)
  (insert "<![CDATA[]]>")
  (let ((index 0))
	(while (< index 3)
	  (backward-char)
	  (setq index (+ index 1)))))

(defun cppimmo-insert-xml-blog ()
  "Insert XML tags that I use for my website's blog."
  (interactive)
  (insert "<?xml version=\"1.0\"?>\n")
  (insert "<post>\n")
  (insert "  <title></title>\n")
  (insert "  <link></link>\n")
  (insert "  <description></description>\n")
  (insert (concat "  <pubDate>" (cppimmo-xml-time-string) "</pubDate>\n"))
  (insert "</post>\n"))

(defun cppimmo-insert-xml-blog-rss-item ()
  "Insert XML blog RSS feed entry item.
Use for inserting relevant tags into the <channel> tag of an existing XML
document."
  (interactive)
  (insert "<item>\n")
  (insert "  <title></title>\n")
  (insert "  <link></link>\n")
  (insert "  <filename>.xml</filename>\n")
  (insert "  <description><description>\n")
  (insert (concat "  <pubDate>" (cppimmo-xml-time-string) "</pubDate>\n"))
  (insert "</item>"))
