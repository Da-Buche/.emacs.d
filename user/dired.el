;; ===============================================================================================================
;; Personal setup for dired mode
;; 
;; Author: Aurelien Buchet
;; ===============================================================================================================

(require 'dired)

;; Use find-alternate-file by default instead of find-file
(define-key dired-mode-map (kbd "e")   'dired-find-alternate-file)
(define-key dired-mode-map (kbd "f")   'dired-find-alternate-file)
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)

;; Also swap find-file key
(define-key dired-mode-map (kbd "a") 'dired-find-file)

;; Replace go up accordingly
(define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))


