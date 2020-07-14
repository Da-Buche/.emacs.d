;; ===============================================================================================================
;; Custom latex mode
;; 
;; A. Buchet 
;; ===============================================================================================================

;; latex default pdf
;(setq TeX-PDF-mode t)

(require 'cl-macs)

(for i 0 2 (insert (format "%d" i)))

(cl-defun latex-custom-print (&key (file   (progn (save-buffer) (buffer-file-name)))
                                   (latex  "xelatex")
                                   twice)
  "Print FILE using LATEX, delete extra generated files"
  (interactive)
  (let ((name (file-name-sans-extension file)))
    ;; Run latex
    (for i 0 (if twice 2 1)
      (shell-command-to-string (format "%s -interaction=nonstopmode %s" latex file)))
    ;; Delete untimely generated files
    (dolist (suffix '("aux" "log" "out")) 
      (delete-file (concat name "." suffix)))
    ));

(add-hook 'latex-mode-hook 
  (lambda nil
    (local-set-key (kbd "C-x p") 'latex-custom-print)))


