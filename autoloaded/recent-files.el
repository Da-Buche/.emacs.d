;; ===============================================================================================================
;; Register recent files
;; 
;; A. Buchet 
;; ===============================================================================================================

(require 'recentf)

;; enable recent files mode.
(recentf-mode t)

;; 100 files ought to be enough.
(setq recentf-max-saved-items 100)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;; get rid of `find-file-read-only' and replace it with something
;; more useful.
;(global-set-key (kbd "C-x C-r") 'ido-recentf-open)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; Open recent files in other window
(defun recentf-open-files-other-window nil
  "Show a dialog to open a recent file in other window."
  (interactive)
  (other-window 1)
  (recentf-open-files))

(global-set-key (kbd "C-x C-4 C-r" ) 'recentf-open-files-other-window)
(global-set-key (kbd "C-x 4 C-r"   ) 'recentf-open-files-other-window)


