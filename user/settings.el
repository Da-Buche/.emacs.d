;; =============================================================================================================
;; Personal Emacs Setup
;; 
;; Author: Aurélien Buchet
;; =============================================================================================================

(defalias 'yes-or-no-p 'y-or-n-p)

(add-to-list 'auto-mode-alist `(,(regexp-opt '(".myalias" ".bash" ".csh" ".tcsh")) . shell-script-mode))

;; =============================================================================================================
;; Default Font, Face and Background
;; =============================================================================================================

;; Setting font white and background gray
(set-face-foreground 'default "linen")
(set-face-background 'default "gray15")

;; Setting bold font faces 
(set-frame-font "DejaVu Sans Mono-10")

(defun set-frame-font-size (&optional n)
  "Set frame font size to N"
  (interactive "n")
  (set-frame-font (format "DejaVu Sans Mono-%d" (or n 12)) t))

;; Remove Menu bar, tool bar and scroll bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

;; Show line-number
(global-linum-mode 1)

;; Display columns by default
(column-number-mode 1)

;; Truncate long lines (do no display them on several lines)
(setq-default truncate-lines t)

;; Replaces tabulations with spaces 
(setq-default indent-tabs-mode nil)
(setq-default x-stretch-cursor t)

;; Enables auto-revert
(setq-default global-auto-revert-mode t)

;; Backup in one place. flat, no tree structure
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

;; Replaces cursor when scrolling or using page Up/Down
(setq scroll-preserve-screen-position 'always)

;; Case insensitive file search
(setq read-file-name-completion-ignore-case t)

;; =================================================================================
;; Existing Modes
;; =================================================================================

;; Replace audio bell by visual one
(setq visible-bell t)

;; C-x in term-mode
(add-hook 'term-mode-hook
  (lambda ()
    ;; Hack to set two escape chars.
    (let (term-escape-char) (term-set-escape-char ?\C-x))
    (let (term-escape-char) (term-set-escape-char ?\C-c))))

(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; Highlighting associated parentheses
(show-paren-mode 1)

;; IDO mode (interactive find-file and buffer)
(ido-mode 1)
;(ido-everywhere 1)

;; Showing changes
;(global-highlight-changes-mode 1)
;(set-face-foreground 'highlight-changes "orange red")
;(set-face-foreground 'highlight-changes-delete "orange red")
;(global-highlight-changes-mode 0)
;(global-set-key (kbd "C-c") 'highlight-changes-mode)

;; Allowing Outline
(outline-minor-mode 1)

;; Outline-minor-mode key map
(define-prefix-command 'cm-map nil "Outline")

;; HIDE
(define-key cm-map "q" 'outline-hide-sublevels)           ; Hide everything but the top-level headings
(define-key cm-map "t" 'outline-hide-body)                ; Hide everything but headings (all body lines)
(define-key cm-map "o" 'outline-hide-other)               ; Hide other branches
(define-key cm-map "c" 'outline-hide-entry)               ; Hide this entry's body
(define-key cm-map "l" 'outline-hide-leaves)              ; Hide body lines in this entry and sub-entries
(define-key cm-map "d" 'outline-hide-subtree)             ; Hide everything in this entry and sub-entries

;; SHOW
(define-key cm-map "a" 'outline-show-all)                 ; Show (expand) everything
(define-key cm-map "e" 'outline-show-entry)               ; Show this heading's body
(define-key cm-map "i" 'outline-show-children)            ; Show this heading's immediate child sub-headings
(define-key cm-map "k" 'outline-show-branches)            ; Show all sub-headings under this heading
(define-key cm-map "s" 'outline-show-subtree)             ; Show (expand) everything in this heading & below

;; MOVE
(define-key cm-map "u" 'outline-up-heading)               ; Up
(define-key cm-map "n" 'outline-next-visible-heading)     ; Next
(define-key cm-map "p" 'outline-previous-visible-heading) ; Previous
(define-key cm-map "f" 'outline-forward-same-level)       ; Forward - same level
(define-key cm-map "b" 'outline-backward-same-level)      ; Backward - same level

(global-set-key "\M-o" cm-map)

;; Map for smerge
(setq smerge-mode-map
  (let ((keymap (make-keymap)) pair key callback)
    (dolist (pair '(
          ("n" smerge-next)
          ("u" smerge-keep-upper)
          ("l" smerge-keep-lower)
          )) (setq key (pop pair)) (setq callback (pop pair)) (define-key keymap (eval `(kbd ,key)) callback))
    ;; Returns the keymap
    keymap))


;; =============================================================================================================
;; Resize Emacs properly
;; =============================================================================================================
;; Ignored as emacs setup is saving desktop between sessions
;(when (boundp 'frame-resize-pixelwise)
;  (setq-default frame-resize-pixelwise t)
;  (set-frame-size (selected-frame) 1875 925 t)
;  (set-frame-position (selected-frame) 1920 31))


;; Open clicked help in *Help* buffer window
;; Not done yet

