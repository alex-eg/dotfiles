(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(setq-default indent-tabs-mode nil)

;;; Local CLHS browsing!
(load "~/quicklisp/clhs-use-local.el" t)

(use-package use-package
  :init
  (setq use-package-always-ensure t))

(use-package sly
  :ensure t
  :custom
  (inferior-lisp-program "sbcl")
  :config (sly-define-common-lisp-style "lbge"
            (:inherit "classic")
            (:indentation
             (λ (as lambda))))
  :config (defun set-common-lisp-style ()
            (setq-local common-lisp-style "lbge"))
  :hook ((lisp-mode . sly-mode)
         (lisp-mode . set-common-lisp-style)))

(use-package elisp-mode
  :custom
  (lisp-indent-function 'lisp-indent-function))

(setq-default c-basic-offset 4)

(set-face-attribute 'default nil :font "Input Mono-10")

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(use-package tango-plus-theme
  :config
  (load-theme 'tango-plus))

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

(use-package magit
  :ensure t
  :delight
  :custom
  (magit-bury-buffer-function #'kill-buffer)
  :bind
  (:map mode-specific-map
        :prefix-map magit-prefix-map
        :prefix "m"
        ("b" . #'magit-blame-addition)
        ("B" . #'magit-branch-create)
        ("c" . #'magit-checkout)
        ("C" . #'magit-commit-create)
        ("f" . #'magit-find-file)
        ("l" . #'magit-log-buffer-file)))

(use-package company
  :bind (("C-<tab>" . company-complete))
  :hook (after-init . global-company-mode))

(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :config (setq org-log-done t)
  (setq org-agenda-files '("~/sync/org.org"
                           "~/sync/todo.org")
        org-refile-targets '(("~/sync/org.org" :maxlevel . 3)
                             ("~/sync/todo.org" :maxlevel . 2))))

(use-package org-journal
  :init (setq
         org-journal-file-type 'yearly
         org-journal-dir "~/dev/journal")
  :hook (org-journal-mode . (lambda ()
                              (set-fill-column 80)
                              (auto-fill-mode))))

(use-package reverse-im
  :config
  (reverse-im-activate "russian-computer"))

(use-package which-key
  :config (which-key-mode))

;;; Eglot
(use-package eglot
  :hook ((c-mode c++-mode objc-mode) .
         'eglot-ensure)
  :config (add-to-list 'eglot-server-programs
                       '((c++-mode c-mode objc-mode) "clangd")))

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
 ((kbd "C-x w") . #'switch-to-buffer-other-window)
 ((kbd "C-c C-r") . #'ivy-resume))
 ((kbd "C-c l") . "λ"))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(transient-mark-mode -1)
(column-number-mode t)
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
  :commands paredit-mode
  :hook (clojure-mode lisp-mode emacs-lisp-mode))

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

(define-advice kill-ring-save (:before (beg end &optional region) pulse-kill-save)
  (save-mark-and-excursion
    (let ((beg (if region (region-beginning) beg))
          (end (if region (region-end) end)))
      (pulse-momentary-highlight-region beg end 'region))))
