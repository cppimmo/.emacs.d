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
