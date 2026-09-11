;; ===============================================================================================================
;; `vterm` setup
;; 
;; A. Buchet - September 2026
;; ===============================================================================================================

(use-package vterm :load-path "/home/abuchet/projects/emacs-libvterm/")

(require 'vterm)

(setq vterm-shell "tcsh")

(define-key vterm-mode-map (kbd "C-u")
  (lambda () (interactive) (vterm-send-key "u" nil nil t)))

(defun ensure-third-window ()
  "If only 2 windows and display width >= 1728, split to create a 3rd, then balance.
Return the third (rightmost) window."
  (let ((ws (sort (window-list) (lambda (a b) (< (car (window-edges a)) (car (window-edges b)))))))
    (when (and (= 2 (length ws)) (>= (display-pixel-width) 1728))
      (select-window (car (last ws)))
      (split-window-right)
      (balance-windows))
    (let ((ws (sort (window-list) (lambda (a b) (< (car (window-edges a)) (car (window-edges b)))))))
      (nth 2 ws))))

(defun go-to-third-window ()
  "Move cursor to the third window, creating it if needed."
  (interactive)
  (let ((w (ensure-third-window)))
    (when w (select-window w))))

(defun chipagents ()
  "Open chipagents. If visible, go to it; otherwise use the third window."
  (interactive)
  (let ((buf (get-buffer "*chipagents*")))
    (if (and buf (get-buffer-window buf))
        (select-window (get-buffer-window buf))
      (let ((w (ensure-third-window)))
        (when w (select-window w)))
      (if (and buf (buffer-live-p buf))
          (switch-to-buffer buf)
        (let ((vterm-shell "tcsh"))
          (vterm "*chipagents*")
          (vterm-send-string ",chipagents")
          (vterm-send-return))))))

(defun vterm-third ()
  "Open vterm. If visible, go to it; otherwise use the third window."
  (interactive)
  (let ((buf (get-buffer "*vterm*")))
    (if (and buf (get-buffer-window buf))
        (select-window (get-buffer-window buf))
      (let ((w (ensure-third-window)))
        (when w (select-window w)))
      (if (and buf (buffer-live-p buf))
          (switch-to-buffer buf)
        (vterm "*vterm*")))))

(advice-add 'vterm :around
            (lambda (orig-fn &rest args)
              (if (called-interactively-p 'interactive)
                  (vterm-third)
                (apply orig-fn args))))

(global-set-key (kbd "C-x 9")   'go-to-third-window)

(provide 'chipagents)

