(require 'package)
(package-initialize)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

;;; XEmacs backwards compatibility file


(setq user-init-file
    (expand-file-name "init.el"
			(expand-file-name ".emacs.d" "~")))

(add-to-list 'load-path "~/.emacs.d/third-party")

(load-file user-init-file)

