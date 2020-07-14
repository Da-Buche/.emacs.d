;; ===============================================================================================================
;; Functions to edit current buffer file
;; 
;; A. Buchet 
;; ===============================================================================================================

;; -------------------------------------------------------
;; Rename current file
;; 
;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
;; edited so current file name is used as default input
;; -------------------------------------------------------
(defun rename-current-file (new-name)
  "Rename both current buffer and file it's visiting to NEW-NAME."
  (interactive (let ((file-name (car (last (split-string (buffer-file-name) "/")))))
                 (list (read-string (format "New Name (%s): " file-name) file-name nil file-name))))
  (let ((file-name (buffer-file-name)))
    (if (not file-name) (message "Buffer '%s' is not visiting a file!" (buffer-name))
      (if (get-buffer new-name) (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file file-name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;; -------------------------------------------------------
;; Move current file
;; 
;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
;; -------------------------------------------------------
(defun move-current-file (dir)
  "Move both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name     (buffer-name))
         (filename (buffer-file-name))
         (dir      (if (string-match dir "\\(?:/\\|\\\\)$") (substring dir 0 -1) dir))
         (newname  (concat dir "/" name)))
    (if (not filename) (message "Buffer '%s' is not visiting a file!" name)
      (progn
        (copy-file filename newname 1)
        (delete-file filename)
        (set-visited-file-name newname)
        (set-buffer-modified-p nil)
        t))))

;; -------------------------------------------------------
;; Delete current file
;; -------------------------------------------------------
(defun delete-current-file nil
  "Delete current file and close current buffer"
  (interactive)
  (delete-file (buffer-file-name))
  (kill-buffer (current-buffer)))

;; -------------------------------------------------------
;; Copy current file
;; -------------------------------------------------------
(defun copy-current-file (new-name)
  "Copy current file and switch buffer to display it"
  (interactive (let ((file-name (car (last (split-string (buffer-file-name) "/")))))
                 (list (read-string (format "New Name (%s): " file-name) file-name nil file-name))))
  (copy-file (buffer-file-name) new-name)
  (find-file new-name))

;; -------------------------------------------------------
;; Load current file
;; -------------------------------------------------------
(defun load-current-file nil
  "Load current file"
  (interactive)
  (load-file (buffer-file-name)))

(define-key lisp-interaction-mode-map (kbd "C-x p") 'load-current-file)
(define-key emacs-lisp-mode-map       (kbd "C-x p") 'load-current-file)



