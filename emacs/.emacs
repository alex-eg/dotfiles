(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(setq-default indent-tabs-mode nil)

;;; Local CLHS browsing!
(load "~/quicklisp/clhs-use-local.el" t)

(use-package sly)

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

(use-package counsel
  :ensure t
  :delight
  :defer nil
  :bind (([remap menu-bar-open] . counsel-tmm)
         ([remap insert-char] . counsel-unicode-char)
         ([remap isearch-forward] . counsel-grep-or-swiper)
         ([remap isearch-backward] . counsel-grep-or-swiper))
  :config
  (counsel-mode))

(use-package ivy
  :ensure t
  :delight
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-re-builders-alist '((t . ivy-regex-plus) (t . ivy--regex-fuzzy)))
  (ivy-count-format "%d/%d " "Show anzu-like counter.")
  (ivy-use-selectable-prompt t "Make the prompt line selectable")
  :bind ("C-c C-r" . ivy-resume)
  :config
  (ivy-mode t))

(use-package ivy-hydra
  :ensure t)

(use-package ivy-rich
  :ensure t
  :after ivy
  :config
  (ivy-rich-mode))

(use-package counsel-projectile
  :ensure t
  :after counsel projectile
  :config
  (counsel-projectile-mode))

(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-enable-caching t)
  (projectile-mode 1))

(use-package magit)

(use-package company
  :bind (("C-<tab>" . company-complete))
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package ccls
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp))))

(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :config (setq org-log-done t)
  (setq org-agenda-files
        (list "~/sync/org/todo.org")))

(use-package org-journal
  :init (setq org-journal-dir "~/dev/journal"))

(use-package reverse-im
  :config
  (reverse-im-activate "russian-computer"))

(use-package which-key
  :config (which-key-mode))

;;; LSP
(use-package lsp-mode
  :commands lsp
  :init
  (add-hook 'rust-mode-hook #'lsp))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config (setq lsp-ui-doc-enable nil
                lsp-ui-peek-enable t
                lsp-ui-sideline-enable nil
                lsp-ui-imenu-enable nil
                lsp-ui-flycheck-enable t)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ([remap xref-find-apropos] . lsp-ui-peek-workspace-symbol)))

(use-package company-lsp
  :commands company-lsp)

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

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-offset 'case-label 0)
            (c-set-offset 'inlambda 0)
            (c-set-offset 'inline-open 0)
            (c-set-offset 'innamespace 0)))
