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
  ;; Commands.
  (global-set-key (kbd "C-x 5 M-x") #'cppimmo/execute-command-other-frame)
  (global-set-key (kbd "C-x 5 C-x C-b") #'cppimmo/buffer-menu-other-frame)
  (global-set-key (kbd "C-x 5 C-x r l") #'cppimmo/bookmark-bmenu-other-frame)
  ;; Set binding for whitespace-mode minor mode.
  (global-set-key (kbd "C-M-y") 'whitespace-mode)
  ;; Set bindings for dired.
  (define-key dired-mode-map "F" #'cppimmo/dired-open-in-new-frame)
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
  ;; Set bindings for ERC.
  (global-set-key (kbd "H-e r c") #'cppimmo/launch-erc)
  ;; Bind keys for Helpful.
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h F") #'helpful-function) ; Replace default: info-goto-emacs-command-node.
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h C") #'helpful-command) ; Replace default: describe-coding-system.
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
  (eval-after-load 'emacs-lisp-mode
    (lambda ()
      (define-key emacs-lisp-mode-map (kbd "C-c C-d") #'helpful-at-point)))
  ) ; End of cppimmo/bind-keys-mode.
