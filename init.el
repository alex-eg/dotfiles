(setq inferior-lisp-program "/usr/bin/sbcl") ; your Lisp system
(add-to-list 'load-path "~/programming/lisp/slime")  ; your SLIME directory
(require 'slime)
(slime-setup '(slime-fancy))

;;; Erlang mode
(setq load-path (cons  "/usr/lib/erlang/lib/tools-2.6.6.4/emacs"
			load-path))
(setq erlang-root-dir "/usr/lib/erlang")
(setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
(require 'erlang-start)

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

;;; COOL-mode
(autoload 'cool-mode "cool-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.cool\\'" . cool-mode))

;;; yacc/bison-mode
(autoload 'bison-mode "bison-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.y\\'" . bison-mode))

;;; Flex mode
(autoload 'flex-mode "flex-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.flex\\'" . flex-mode))

;;; GLSL-mode
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))

(setq-default c-basic-offset 4)
(setq column-number-mode t)
(global-set-key [insert] 'delete-selection-mode) ;; [Ins] 

(require 'no-easy-keys)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode t)
(setq inhibit-startup-screen t)
(desktop-save-mode 1)
(no-easy-keys 1)

(custom-set-variables
 '(haskell-mode-hook '(turn-on-haskell-indentation))
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (tsdh-dark))))

(set-default 'indent-tabs-mode nil)
(add-hook 'erlang-mode-hook '(lambda() (setq indent-tabs-mode nil)))

(set-face-attribute 'default nil :family "Inconsolata" :height 150)
(set-fontset-font "fontset-default" '(#x400 . #x4FF) "Terminus")

(require 'rust-mode)
