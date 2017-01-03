(setq slime-lisp-implementations
      '((sbcl ("/usr/bin/sbcl"))
        (ccl ("/usr/bin/ccl"))))
(setq slime-contribs '(slime-fancy))
(require 'cl)

;;; Local CLHS browsing!
(load "~/quicklisp/clhs-use-local.el" t)

(setq-default c-basic-offset 4)
(global-set-key [insert] 'delete-selection-mode) ;; Overwrite mode

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(transient-mark-mode -1)
(column-number-mode t)
(desktop-save-mode 1)

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(setq-default indent-tabs-mode nil)

(set-face-attribute 'default nil :font "8x13")
(set-fontset-font t 'unicode "unifont-10:antialias=false" nil 'prepend)
(set-fontset-font t 'unicode "MS Mincho-12" nil 'prepend)

(require 'ido)
(ido-mode)

(setq default-input-method "russian-computer"
      browse-url-firefox-new-window-is-tab t
      browse-url-firefox-program "firefox-bin"
      ring-bell-function 'ignore
      ido-enable-flex-matching t
      mouse-yank-at-point t
      inhibit-startup-screen t
      column-number-mode t)

(load-theme 'tango-dark)

;;; Paredit
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(define-key global-map (kbd "C-[ C-[ C-[") nil)
(define-key global-map (kbd "C-x w") #'switch-to-buffer-other-window)
