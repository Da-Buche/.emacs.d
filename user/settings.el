;; Setting font white and background gray
(set-face-foreground 'default "linen")
(set-face-background 'default "gray15")

;; Setting bold font faces 
(set-frame-font "DejaVu Sans Mono-10:bold")

;; Removing Toolbar
(tool-bar-mode -1)

;; Removing start screen
(setq inhibit-startup-screen t)

;; Maximizing Emacs Window
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; saving windows
(desktop-save-mode 1)

;; Replacing tabulations with spaces 
(setq-default indent-tabs-mode nil)

;; Enabling auto-revert
(setq-default global-auto-revert-mode t)

;; When split screens are horizontal
;(setq-default split-height-threshold 500)
;(setq-default split-width-threshold 10)
;(setq-default window-size-fixed nil)
;(setq-default window-min-height 50)
;(setq-default window-min-width 2)

;; =================================================================================
;; Creating Global Shortkeys
;; =================================================================================

;; Remove C-x f shortkey
(global-set-key (kbd "C-x f") 'find-file)

;; Kill all special buffers
(defun kill-buffers ()
  "Killing the current buffer and all the special ones (returns to one window display)"
  (interactive)
  ;; kill the current buffer
  (kill-buffer)
  ;;fetching buffers
  (let (buffer (buffers (buffer-list)))
    (while (setq buffer (pop buffers))
      ;; killing only the special ones with *name*
      (when (string-match "\*.*\*" (buffer-name buffer))
        (kill-buffer buffer)
      )
    )
    ;;displaying only the current window
    (delete-other-windows)
  );let
);defun

(global-set-key (kbd "C-x C-k") 'kill-buffers)

;; Deleting backward and forward
(defun global-delete-backward ()
  (interactive)
  (if (eq (char-before) ?\ )
    (while (eq (char-before) ?\ )
      (delete-backward-char 1)
    )
    (delete-backward-char 1)
  );if
);defun

(defun global-delete-forward ()
  (interactive)
  (if (eq (char-after) ?\ )
    (while (eq (char-after) ?\ )
      (delete-char 1)
    )
    (delete-char 1)
  );if
);defun

(global-set-key (kbd "<backspace>") 'global-delete-backward)
(global-set-key (kbd "C-d") 'global-delete-forward)
(global-set-key (kbd "<delete>") 'global-delete-forward)

(defun delete-spaces ()
  "deleting all spaces of the line"  
  (interactive)
  (save-excursion
    (beginning-of-line)
    (while (not (eolp))
      (when (eq (char-after) ?\s)
        (delete-region (point) (+ (point) 1))
      );when
      (forward-char)
    );while
  );save-excursion
);defun

;; Inserting comments
(defun global-insert-comment ()
  "Insert two ; and a space after"
  (interactive)
  (let (beg end)
    (save-excursion
      (beginning-of-line)
      (setq beg (point))
      (end-of-line)
      (setq end (point))
    );save-excursion
    (if (string-match "^[\s\n]*$" (buffer-substring beg end))
      (progn
        (insert (concat comment-start comment-start))
        (insert " ")
        (save-excursion
          (insert comment-end)
        );save-excursion
      );progn
      (insert comment-start)
    );if
  );let
);defun

(global-set-key (kbd ";") 'global-insert-comment)

(defun global-insert-title ()
  (interactive)
  (let ((title (read-string "Title ")))
    (unless (bobp) (insert "\n"))
    (insert (concat comment-start comment-start " =================================================================================\n"))
    (insert (concat comment-start comment-start) " " title "\n")
    (insert (concat comment-start comment-start " =================================================================================\n"))
    ; (next-line)
  );let
);defun

(global-set-key (kbd "C-;") 'global-insert-title)

;; =================================================================================
;; Loading Modes
;; =================================================================================

(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; Highlighting associated parentheses
(show-paren-mode 1)

;; Showing line-number
(global-linum-mode 1)

;; Showing changes
(global-highlight-changes-mode 1)
(set-face-foreground 'highlight-changes "orange red")
(set-face-foreground 'highlight-changes-delete "orange red")
(global-highlight-changes-mode 0)
(global-set-key (kbd "C-c") 'highlight-changes-mode)

;; Allowing Outline
(outline-minor-mode 1)

;; Outline-minor-mode key map
 (define-prefix-command 'cm-map nil "Outline")
 ;; HIDE
 (define-key cm-map "q" 'hide-sublevels)    ; Hide everything but the top-level headings
 (define-key cm-map "t" 'hide-body)         ; Hide everything but headings (all body lines)
 (define-key cm-map "o" 'hide-other)        ; Hide other branches
 (define-key cm-map "c" 'hide-entry)        ; Hide this entry's body
 (define-key cm-map "l" 'hide-leaves)       ; Hide body lines in this entry and sub-entries
 (define-key cm-map "d" 'hide-subtree)      ; Hide everything in this entry and sub-entries
 ;; SHOW
 (define-key cm-map "a" 'show-all)          ; Show (expand) everything
 (define-key cm-map "e" 'show-entry)        ; Show this heading's body
 (define-key cm-map "i" 'show-children)     ; Show this heading's immediate child sub-headings
 (define-key cm-map "k" 'show-branches)     ; Show all sub-headings under this heading
 (define-key cm-map "s" 'show-subtree)      ; Show (expand) everything in this heading & below
 ;; MOVE
 (define-key cm-map "u" 'outline-up-heading)                ; Up
 (define-key cm-map "n" 'outline-next-visible-heading)      ; Next
 (define-key cm-map "p" 'outline-previous-visible-heading)  ; Previous
 (define-key cm-map "f" 'outline-forward-same-level)        ; Forward - same level
 (define-key cm-map "b" 'outline-backward-same-level)       ; Backward - same level
 (global-set-key "\M-o" cm-map)
