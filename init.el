(set-face-attribute 'default nil :family "Inconsolata" :height 160)

;;; Erlang mode
(setq load-path (cons  "/usr/lib/erlang/lib/tools-2.6.6.4/emacs"
			load-path))
(setq erlang-root-dir "/usr/lib/erlang")
(setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
(require 'erlang-start)

(require 'vala-mode)

;;; Forth mode
(autoload 'forth-mode "gforth.el")
     (setq auto-mode-alist (cons '("\\.fs\\'" . forth-mode)
     			    auto-mode-alist))
     (autoload 'forth-block-mode "gforth.el")
     (setq auto-mode-alist (cons '("\\.fb\\'" . forth-block-mode)
     			    auto-mode-alist))
     (add-hook 'forth-mode-hook (function (lambda ()
        ;; customize variables here:
        (setq forth-indent-level 4)
        (setq forth-minor-indent-level 2)
        (setq forth-hilight-level 3)
        ;;; ...
     )))

;;; GLSL-mode
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))

(setq-default c-basic-offset 4)
(setq column-number-mode t)
(global-set-key [insert] 'delete-selection-mode) ;; [Ins] 


(require 'color-theme-solarized)
(require 'no-easy-keys)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode t)
(setq inhibit-startup-screen t)

(no-easy-keys 1)
(color-theme-solarized-dark)
