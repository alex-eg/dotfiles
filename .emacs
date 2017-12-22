(setq slime-lisp-implementations
      '((sbcl ("/usr/local/bin/sbcl"))))
(setq slime-contribs '(slime-fancy))
(require 'cl)

;;; Local CLHS browsing!
(load "~/quicklisp/clhs-use-local.el" t)

(setq-default c-basic-offset 4)

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(setq-default indent-tabs-mode nil)

(set-face-attribute 'default nil :font "Input Sans-10")

(defun set-monospace-font ()
  (buffer-face-set '(:family "Input Mono")))

(defun add-hook-modes (mode-hook-list hook)
  (dolist (mode-hook mode-hook-list)
    (add-hook mode-hook hook)))

(require 'helm)
(require 'helm-config)
(helm-mode 1)
(helm-projectile-on)

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
 ((kbd "M-x") . #'helm-M-x)
 ((kbd "C-x C-f") . #'helm-find-files)
 ((kbd "C-x b") . #'helm-mini)
 ((kbd "M-y") . #'helm-show-kill-ring))

(add-hook-modes '(dired-mode-hook
                       help-mode-hook
                       minibuffer-setup-hook)
                #'set-monospace-font)

(add-hook-modes '(lisp-mode-hook
                  clojure-mode-hook
                  emacs-lisp-mode-hook
                  lisp-interaction-mode-hook)
                #'enable-paredit-mode)

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(transient-mark-mode -1)
(column-number-mode t)
(desktop-save-mode 1)
(winner-mode 1)
(projectile-mode 1)
(electric-indent-mode -1)

(setq default-input-method "russian-computer"
      browse-url-firefox-new-window-is-tab t
      browse-url-firefox-program "firefox-bin"
      ring-bell-function 'ignore
      mouse-yank-at-point t
      inhibit-startup-screen t
      column-number-mode t
      c-default-style "k&r")

(load-theme 'tango-dark)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; ERC settings
;; Kill buffers for channels after /part
(setq erc-kill-buffer-on-part t)
;; Kill buffers for private queries after quitting the server
(setq erc-kill-queries-on-quit t)
;; Kill buffers for server messages after quitting the server
(setq erc-kill-server-buffer-on-quit t)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(put 'narrow-to-region 'disabled nil)

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))
            (c-set-offset 'case-label 0)
            (c-set-offset 'inline-open 0)
            (c-set-offset 'innamespace 0)))

(add-hook 'ggtags-mode-hook
          (lambda ()
            (define-key ggtags-navigation-map (kbd "M-o") nil)
            (define-key ggtags-navigation-map (kbd "M->") nil)
            (define-key ggtags-navigation-map (kbd "M-<") nil)))
