;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-interval 0.5)
 '(fill-column 85)
 '(js-indent-level 2)
 '(ns-alternate-modifier 'super)
 '(ns-command-modifier 'meta)
 '(package-selected-packages '(dockerfile-mode markdown-mode dash ##))
 '(python-indent-offset 2)
 '(sh-basic-offset 2)
 '(split-height-threshold nil)
 '(tab-width 2))

(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region          'disabled nil)

;; Load user setup
(load "~/.emacs.d/load.el")

;; Set `eldo-mode' as default one for SPICE related files 
(autoload 'eldo-mode "~/.emacs.d/modes/eldo-mode.el" nil t)
(add-to-list 'auto-mode-alist `(,(regexp-opt '(".bkp" ".cir" ".ckt")) . eldo-mode))

;; Remove window manager frame decoration
(add-to-list 'default-frame-alist '(undecorated . t))

;; Make sure emacs does not split frame by itself
;(set-frame-parameter nil 'unsplittable t)

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/")
             )
;(package-initialize)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
