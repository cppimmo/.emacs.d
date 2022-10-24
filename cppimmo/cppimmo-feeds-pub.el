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
;;; Public RSS Feeds.
;;
(defun cppimmo/append-to-elfeeds (@feed-list)
  "Append @FEED-LIST to the elfeeds feed list.
Use quoted list and refer to elfeeds documentation."
  (setq elfeed-feeds (append elfeed-feeds @feed-list)))

(cppimmo/append-to-elfeeds
	  ;; The extra symbols are relevant tags.
	  '(("https://www.slackware.com/~alien/multilib/ChangeLog.rss" slackware)
		("https://mirrors.slackware.com/feeds/slackware64-current.rss" slackware)
		("https://docs.slackware.com/feed.php" slackware docs)
		("https://slackbuilds.org/rss/ChangeLog.rss" slackware)
		("http://marav8.free.fr/slackware64-15.0.rss" slackware)
		("https://www.reddit.com/r/slackware.rss" slackware reddit)
		("https://www.reddit.com/r/emacs.rss" emacs reddit)
		("https://www.reddit.com/r/lisp.rss" code reddit)
		("https://www.reddit.com/r/cpp.rss" code reddit)
		("http://xahlee.info/emacs/emacs/blog.xml" blog)
		("http://xahlee.info/comp/blog.xml" blog)
		("http://xahlee.info/kbd/keyboard_blog.xml" blog)
		("https://www.yahoo.com/news/rss" news tech)
		("https://news.ycombinator.com/rss" news tech)
		("http://rss.slashdot.org/Slashdot/slashdotMain" news tech)
		("http://feeds.feedburner.com/linuxquestions/latest" forum tech)
		("http://feeds.feedburner.com/linuxquestions/lqnews" tech news forum)
		("https://emacsforosx.com/atom/release" news emacs)
		("https://emacsforosx.com/atom/pretest" news emacs)
		("https://emacsforosx.com/atom/daily" news emacs)
		("https://planet.emacslife.com/atom.xml" news blog emacs)
		("https://www.reddit.com/r/orgmode.rss" emacs reddit)
		("https://oneofus.la/have-emacs-will-hack/feed.xml" emacs)
		("https://thetech.com/feed" news)
		("https://www.moonofalabama.org/index.rdf" news)))
