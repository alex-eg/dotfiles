(setq slime-lisp-implementations
      '((sbcl ("/usr/bin/sbcl"))
        (ccl ("/usr/bin/ccl"))))
(setq slime-contribs '(slime-fancy))
(require 'cl)

;;; Local CLHS browsing!
(load "~/quicklisp/clhs-use-local.el" t)

(setq-default c-basic-offset 4)

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
 ((kbd "C-x w") . #'switch-to-buffer-other-window))

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

(add-hook-modes '(dired-mode-hook
                       help-mode-hook
                       minibuffer-setup-hook)
                #'set-monospace-font)

(add-hook-modes '(lisp-mode-hook
                  emacs-lisp-mode-hook
                  lisp-interaction-mode-hook)
                #'enable-paredit-mode)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(transient-mark-mode -1)
(column-number-mode t)
(desktop-save-mode 1)
(winner-mode 1)
(ivy-mode 1)
(projectile-mode 1)

(setq default-input-method "russian-computer"
      browse-url-firefox-new-window-is-tab t
      browse-url-firefox-program "firefox-bin"
      ring-bell-function 'ignore
      ido-enable-flex-matching t
      mouse-yank-at-point t
      inhibit-startup-screen t
      column-number-mode t
      ido-default-buffer-method 'selected-window
      ido-default-file-method 'selected-window)

(load-theme 'tango-dark)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
