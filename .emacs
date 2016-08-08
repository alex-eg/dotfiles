(setq slime-lisp-implementations
      '((sbcl ("/usr/bin/sbcl"))
        (ccl ("/usr/bin/ccl"))))
(setq slime-contribs '(slime-fancy))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode t)

(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq inhibit-startup-screen t)

(desktop-save-mode 1)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)

(load-theme 'tango-dark t)

(add-to-list 'auto-mode-alist '("\\.asm\\'" . nasm-mode))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(set-face-attribute 'default nil :font "MS Mincho-12")
(set-fontset-font t 'unicode "MS Mincho-12" nil 'prepend)
(set-fontset-font t 'unicode "Unifont 12" nil 'prepend)

(global-set-key (kbd "ESC ESC ESC") nil)

(global-set-key (kbd "C-c C-j") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "C-x w") 'ace-window)

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-default-buffer-method 'selected-window) ;;; I hate ido switching to other frame

(global-set-key (kbd "C-x C-b") 'buffer-menu)

(setq mouse-yank-at-point t)

;;; Set windowd width

(defun set-window-width (n)
  "Set the selected window's width."
  (adjust-window-trailing-edge (selected-window) (- n (window-width)) t))

(defun set-80-columns ()
  "Set the selected window to 80 columns."
  (interactive)
  (set-window-width 80))

(global-set-key "\C-x~" 'set-80-columns)

;;; Rust autocomplete

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)

(add-hook 'racer-mode-hook #'company-mode)

(setq company-tooltip-align-annotations t)
(add-to-list 'exec-path "/home/ex/.cargo/bin/")
