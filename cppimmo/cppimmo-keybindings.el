;;; Keybindings
;;

;; Set the menu app key to hyper.
;; Use Shift+F10 or Ctrl+Shift+F10 instead for application/context menu.
;; Protesilaos blog post about setting the hyper key with xmodmap:
;; https://protesilaos.com/codelog/2019-10-10-debian-xmodmap/
(when (string-equal system-type "gnu/linux")
  (global-set-key (kbd "<menu>") nil))
(when (string-equal system-type "windows-nt")
  (setq w32-pass-apps-to-system nil)
  (setq w32-apps-modifier 'hyper))


(defun cppimmo/bind-keys-g ()
  "Set global keys."
  ;; Set binding for whitespace-mode minor mode.
  (global-set-key (kbd "C-M-y") 'whitespace-mode)
  
  ;; Set binding for hl-line-mode.
  (global-set-key (kbd "H-l b") 'hl-line-mode) ; locally.
  (global-set-key (kbd "H-l g") 'global-hl-line-mode) ; Globally.
  ;; Cycle themes.
  (global-set-key (kbd "H-t") #'cppimmo/cycle-custom-themes)
  ;; Set bindings for windmove.
  (global-set-key (kbd "H-w l") 'windmove-swap-states-left)
  (global-set-key (kbd "H-w r") 'windmove-swap-states-right)
  (global-set-key (kbd "H-w u") 'windmove-swap-states-up)
  (global-set-key (kbd "H-w d") 'windmove-swap-states-down)
  ) ; End of cppimmo/bind-keys-global.

(defun cppimmo/bind-keys-m ()
  "Set keys for certain modes."
  ;; Set special bindings for the default nxml-mode.
  (eval-after-load 'nxml-mode
	(lambda ()
	  ;; Set binding for CDATA tag insertion for XML documents.
	;;; See cppimmo/cppimmo-xml.el
	  (define-key nxml-mode-map (kbd "C-c M-!") 'cppimmo/xml-insert-cdata)
	  ;; Set binding for blog insertion for XML documents.
	  (define-key nxml-mode-map (kbd "C-c M-@") 'cppimmo/xml-insert-blog)
	  ;; Set binding for RSS feed item insertion for XML documents.
	  (define-key nxml-mode-map (kbd "C-c M-#") 'cppimmo/xml-insert-blog-rss-item)))
  ) ; End of cppimmo/bind-keys-mode.
