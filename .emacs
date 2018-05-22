(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(setq-default indent-tabs-mode nil)

(setq slime-lisp-implementations
      '((sbcl ("/usr/bin/sbcl"))))
(setq slime-contribs '(slime-fancy))
(require 'cl)

;;; Local CLHS browsing!
(load "~/quicklisp/clhs-use-local.el" t)

(setq-default c-basic-offset 4)

(set-face-attribute 'default nil :font "Input Mono-10")

(defun add-hook-modes (mode-hook-list hook)
  (dolist (mode-hook mode-hook-list)
    (add-hook mode-hook hook)))

(use-package use-package
  :init
  (setq use-package-always-ensure t))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(use-package alect-themes
  :config
  (load-theme 'alect-black))

(use-package magit)

(use-package projectile
  :config
  (projectile-mode 1))

(use-package helm
  :demand t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)
         ("M-y" . helm-show-kill-ring))
  :config
  (helm-mode 1))

(use-package helm-projectile
  :config
  (helm-projectile-on))

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package rtags
  :config
  (rtags-enable-standard-keybindings))

(use-package helm-rtags
  :config
  (setq rtags-display-result-backend 'helm))

(use-package company-rtags
  :init
  (setq rtags-autostart-diagnostics t)
  (setq rtags-completions-enabled t)
  :config
  (push 'company-rtags company-backends))

(use-package flycheck-rtags)

(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :config (setq org-log-done t)
  (setq org-agenda-files
        (list "~/sync/org/todo.org")))

(use-package reverse-im
  :config
  (reverse-im-activate "russian-computer"))

(use-package which-key
  :config (which-key-mode))

(defmacro global-set-keys (&rest map)
  (let (a)
    (dolist (key-action map)
      (push `(global-set-key ,(car key-action) ,(cdr key-action))
            a))
    `(progn ,@a)))

(global-set-keys
 ([insert] . 'delete-selection-mode) ;; Overwrite mode
 ((kbd "M-o") . 'other-window)       ;; Instead of C-x o
 ((kbd "C-[ C-[ C-[") . nil)
 ((kbd "C-x /") . #'replace-string)
 ((kbd "C-x w") . #'switch-to-buffer-other-window))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(transient-mark-mode -1)
(column-number-mode t)
(desktop-save-mode 1)
(winner-mode 1)
(electric-indent-mode -1)

(setq default-input-method "russian-computer"
      browse-url-browser-function 'browse-url-firefox
      browse-url-firefox-new-window-is-tab t
      browse-url-firefox-program "firefox"
      ring-bell-function 'ignore
      mouse-yank-at-point t
      inhibit-startup-screen t
      column-number-mode t
      c-default-style "k&r")

(use-package paredit
  :config
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; ERC
(setq erc-kill-buffer-on-part t
      erc-kill-queries-on-quit t
      erc-kill-server-buffer-on-quit t)

(put 'narrow-to-region 'disabled nil)

(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-offset 'case-label 0)
            (c-set-offset 'inline-open 0)
            (c-set-offset 'innamespace 0)))
