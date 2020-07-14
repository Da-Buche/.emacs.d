;; =============================================================================================================
;; Startup settings and saving last session desktop
;; =============================================================================================================

;; Saves buffers and windows when emacs is closed
(setq-default desktop-dirname "~/.emacs.d/")
(setq-default desktop-path '("~/.emacs.d/"))
(setq-default desktop-load-locked-desktop t)
(setq-default desktop-save t)
(desktop-save-mode 1)
;(when (not (eq (emacs-pid) (desktop-owner))) (desktop-save-mode-off))
  

;; Maximizes Emacs Window
;(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; When split screens are horizontal
;(setq-default split-height-threshold 500)
;(setq-default split-width-threshold 10)
;(setq-default window-size-fixed nil)
;(setq-default window-min-height 50)
;(setq-default window-min-width 2)

;; Removes start screen, bug with desktop save mode
;(setq inhibit-startup-screen t)

;; Disables emacs messages
;(setq-default message-log-max nil)
;(kill-buffer "*Messages*")