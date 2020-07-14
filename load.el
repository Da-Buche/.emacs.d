;; Loading recursively all files from the current directory

(let* ((emacs-settings-root "~/.emacs.d")
       (l-files (cons (concat emacs-settings-root "/user") nil))
       t-file
       );def
  (while (setq t-file (pop l-files))
    ;; adding contained files to list if filename is a directory
    (when (and (not (equal "." t-file))
	       (not (equal ".." t-file))
	       (file-directory-p t-file)
	       )
      (setq l-files (append l-files (directory-files t-file t "[^~.]$")))
      );when
    (when (and (< 3 (length t-file))
	       (equal (substring t-file -3) ".el")
	       )
      (load t-file)
      )))
