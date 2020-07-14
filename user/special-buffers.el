;; ================================================================================================================
;; Special buffers
;; 
;; Author: Aurelien Buchet
;; ================================================================================================================

;; List all buffers in current window instead of other one
(global-set-key "\C-x\C-b" 'buffer-menu) ; default is list-buffers

(defvar everlasting-buffers nil "List of special buffers that cannot be deleted")

;; =============================================================================================================
;; Scratch buffer
;; 
;; Replace default scratch buffer with one associated to a file
;; =============================================================================================================
(defun scratch nil "Find or creates scratch buffer" (interactive)
  (let ((scratch (get-buffer "*scratch*")))
    (if scratch (switch-to-buffer scratch)
      (find-file "~/.emacs.d/scratch") (rename-buffer "*scratch*") (skill-mode))))

;(add-hook (symcat initial-major-mode '-hook)
;  (lambda nil (when (and (equal (buffer-name) "*scratch*") (not (buffer-file-name)))
;    (kill-buffer "*scratch*") (scratch))))

(push "*scratch*" everlasting-buffers)

;; ================================================================================================================
;; Kill Special buffers except evelasting ones
;; ================================================================================================================
(defun kill-buffers nil "Kill special buffers, place two vertical windows showing different buffers"
  (interactive)
  ;; Fetch buffers
  (let (buffer (buffers (buffer-list)) name left right)
    ;; Kill only special ones with *name* that are not everlasting
    (while (setq buffer (pop buffers)) (setq name (buffer-name buffer))
      (unless (member name everlasting-buffers) (when (string-match-p "\\*.*\\*" name) (kill-buffer buffer))))
    ;; Fetch left and right windows
    (setq left (or (windmove-find-other-window 'left) 
        (progn (other-window 1) (windmove-find-other-window 'left))))
    (unless left (setq left (get-buffer-window)) (split-window-right))
    (select-window left) (setq right (windmove-find-other-window 'right))
    ;; Change right buffer when both windows are showing the same
    (when (equal (window-buffer left) (window-buffer right)) 
      (select-window right) (next-buffer) (select-window left))
  );let
);defun

;; Kill current buffer
(global-set-key (kbd "C-x k") (lambda nil (interactive) (kill-buffer (current-buffer))))
;; Kill other buffer
(global-set-key (kbd "C-x 4 k") (lambda nil (interactive) (kill-buffer (window-buffer (next-window)))))
;; Kill all special buffers
;(global-set-key (kbd "C-x C-k") 'kill-buffers)

;; Swap right/left buffers with C-tab
(global-set-key (kbd "C-<tab>") 'rotate-buffers)

(defun rotate-buffers (&rest args)
  "Switch left and right buffers"
  (interactive) 
  (if (ignore-errors (buf-move-right) t) (other-window 1) (buf-move-left)))

