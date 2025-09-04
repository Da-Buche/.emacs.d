;; ===============================================================================================================
;; Enhanced multi-occur in all buffers or in buffers matching current-buffer major mode
;; 
;; A. Buchet 
;; ===============================================================================================================

;; -------------------------------------------------------
;; Multi occur in all buffers
;; -------------------------------------------------------
(defun multi-occur-all-buffers nil
  "Show all lines matching REGEXP in every buffer"
  (interactive)
  (multi-occur (buffer-list) (car (occur-read-primary-args))))

;; -------------------------------------------------------
;; Multi occur in all buffers matching current mode
;;
;; From: https://linuxfr.org/news/gnu-emacs-quelques-extensions-premiere-partie
;; but names are modified
;; -------------------------------------------------------
(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let (buffer-mode-matches)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (if (eq mode major-mode)
            (add-to-list 'buffer-mode-matches buf))))
    buffer-mode-matches))

(defun multi-occur-current-mode nil
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur (get-buffers-matching-mode major-mode) (car (occur-read-primary-args))))



