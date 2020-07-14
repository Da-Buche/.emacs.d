;; ===============================================================================================================
;; Emacs shell and term enhancements
;; 
;; A. Buchet 
;; ===============================================================================================================

(require 'shell)
(require 'term)
(require 'dirtrack)

;; -------------------------------------------------------
;; Shell
;; -------------------------------------------------------
(setq-default dirtrack-list '("^.*\\[31;1m\\(.+\\)\\[0m *\n.*>.* " 1 t))

(defun shell-set-paragraph-hook (&rest args)
  "Set paragraph start to match prompt and "
  (setq-local comint-prompt-regexp "\\[.*?\\]\\$")
  (local-set-key (kbd "M-{") 'backward-paragraph)
  )

(add-hook 'shell-mode-hooks 'shell-set-paragraph-hook)

(add-hook 'shell-mode-hook
          (lambda nil
            
            ;; Allow dots in words but remove greater-than character
            (modify-syntax-entry ?\. "w" shell-mode-syntax-table)
            (modify-syntax-entry ?\> "." shell-mode-syntax-table)
            
            ;; Use M-up and M-down to browse matching commands
            (define-key shell-mode-map (kbd "<M-up>")   'comint-previous-matching-input-from-input)
            (define-key shell-mode-map (kbd "<M-down>") 'comint-next-matching-input-from-input)

            (setq pcomplete-ignore-case t)
            
            (shell-dirtrack-mode 0)
            (dirtrack-mode 1)))

;; Clear Shell properly with C-l
(defun shell-clear nil
  "Clear current shell buffer"
  (interactive)
  (let ((comint-buffer-maximum-size 1)) (comint-truncate-buffer) (end-of-line)))

(define-key shell-mode-map (kbd "C-l") 'shell-clear)

;; -------------------------------------------------------
;; Term
;; -------------------------------------------------------

(add-hook 'term-mode-hook
  (lambda nil
    ;(setq term-prompt-regexp "^\[[0-9:]+\].+> ")
    (setq term-prompt-regexp "^> ")
    (setq-local mouse-yank-at-point t)
    (setq-local transient-mark-mode nil)
    ;(setq-local truncate-lines     nil)
    ;(setq term-suppress-hard-newline t)
    ;(auto-fill-mode -1)
    ;(setq term-width 1000)
    ;(setq tab-width 4)
    (set-terminal-coding-system 'utf-8-unix)
    (setq default-process-coding-system '((utf-8-unix . utf-8-unix)))
    ))


(global-set-key (kbd "M-T") (lambda nil (interactive) (other-window 1) (shell)))
                                        ;(term "/bin/tcsh")
(global-set-key (kbd "C-x 4 M-T") (lambda nil (interactive) (shell)))

;; From https://www.emacswiki.org/emacs/ShellMode#toc12 but not working with my emacs

(defun term-switch-to-shell-mode ()
  "Switch from term to shell mode"
  (interactive)
  (if (equal major-mode 'term-mode)
      (progn
        (shell-mode)
        (set-process-filter  (get-buffer-process (current-buffer)) 'comint-output-filter )
        (local-set-key (kbd "C-j") 'term-switch-to-shell-mode)
        (compilation-shell-minor-mode 1)
        (comint-send-input)
      )
    (progn
        (compilation-shell-minor-mode -1)
        (font-lock-mode -1)
        (set-process-filter  (get-buffer-process (current-buffer)) 'term-emulate-terminal)
        (term-mode)
        (term-char-mode)
        (term-send-raw-string (kbd "C-l"))
        )))

(define-key term-raw-map (kbd "C-j") 'term-switch-to-shell-mode)


