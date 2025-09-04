;; ===================================================================
;; Necessary settings, functions and bindkeys to properly work using
;; Emacs
;;
;; A. Buchet
;; ===================================================================

;; ===================================================================
;; Settings according on terminal/graphic mode
;;  - Backup
;;  - Backspace
;;  - Bell
;;  - Server
;;  - Save desktop
;; ===================================================================

;; Setting font white and background gray
(set-face-foreground 'default "linen")
(set-face-background 'default "gray15")

(if (display-graphic-p)
    ;; -----------------------------------------------------
    ;; Graphic mode
    ;; -----------------------------------------------------
    (progn ;; Store backup in one place. Flat, no tree structure
      (setq backup-directory-alist '(("" . "~/.emacs.d/.backup")))
      ;; Replace audio bell by visual one
      (setq visible-bell t)
      ;; Start server mode
      (server-start)
      ;; Save buffers and windows when emacs is closed
      (setq-default desktop-dirname             "~/.emacs.d/"    )
      (setq-default desktop-path                '("~/.emacs.d/") )
      (setq-default desktop-load-locked-desktop t                )
      (setq-default desktop-save                t                )
      (desktop-save-mode 1)
      )
  ;; -----------------------------------------------------
  ;; Terminal mode
  ;; -----------------------------------------------------
  ;; Make Ctrl-backspace works properly
  (global-set-key (kbd "C-h") 'backward-kill-word)
  ;; Make sure no backup are generated
  (setq make-backup-files nil)
  )

;; Use flashing mode line instead of visual bell
(setq visible-bell       nil)
(setq ring-bell-function (lambda () (invert-face 'mode-line) (run-with-timer 0.1 nil #'invert-face 'mode-line)))


;; ===================================================================
;; Miscellaneous
;; ===================================================================

;; Remove Menu bar, tool bar and scroll bar
(menu-bar-mode                    -1)
(tool-bar-mode                    -1)
(toggle-scroll-bar                -1)

;; Show line and column numbers
(global-display-line-numbers-mode  1)
(column-number-mode                1)

;; Highlighting associated parentheses
(show-paren-mode            1)

;; Truncate long lines (do not display them on several lines)
(setq-default truncate-lines t)

;; Replaces tabulations with spaces
(setq-default indent-tabs-mode nil)
(setq-default x-stretch-cursor t)

;; Replaces cursor when scrolling or using page Up/Down
(setq scroll-preserve-screen-position 'always)

;; Case insensitive file search
(setq read-file-name-completion-ignore-case t)

;; Faster yes or no prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; Open specific files extensions with associated modes
(dolist (pair '(("\\.myalias|mycsh|mytcsh|bash|csh|tcsh|ucdprod\\'" . shell-script-mode     )
                ("\\.emacs\\'"                                      . lisp-interaction-mode )
                ("\\.m\\'"                                          . octave-mode           )
                ))
  (add-to-list 'auto-mode-alist pair))

;; Enable useful features (disabled by default and prompting user)l
(put 'scroll-left 'disabled nil)

;; Use bash as default shell, this avoid csh extremely long loading time
(setq-default explicit-shell-file-name "/bin/bash")
(setq-default shell-file-name "/bin/bash")

;; Always follow version controlled symbolic lonks
(setq-default vc-follow-symlinks t)


;; ===================================================================
;; IDO and Smex
;; ===================================================================

;; IDO mode (interactive find-file and buffer)
(ido-mode 1)

;; Smex, IDO for M-x commands
(let ((file "~/.emacs.d/smex.el"))
  (when (file-readable-p file)
    (load-file file)
    (smex-initialize)
    (global-set-key "\M-x" 'smex)
    (global-set-key (kbd "M-X") 'smex-major-mode-commands)
    ))


;; ===================================================================
;; Pesky and Missing bindkeys
;; ===================================================================

;; Unbind Pesky Sleep Button
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])

;; Default find file (without IDO)
(defalias 'find-file-no-remap 'find-file "Find file which is not remapped (by IDO for instance)")
(global-set-key (kbd "C-x f") 'find-file-no-remap)

;; Save file both with C-x s and C-x C-s
(global-set-key (kbd "C-x s") 'save-buffer)

;; Scroll
(global-set-key (kbd "<M-up>"    ) 'scroll-down-line            )
(global-set-key (kbd "<M-down>"  ) 'scroll-up-line              )

;; Resize window
(global-set-key (kbd "<M-left>"  ) 'shrink-window-horizontally  )
(global-set-key (kbd "<M-right>" ) 'enlarge-window-horizontally )

;; Fetch previous mark
(global-set-key (kbd "C-v" ) 'pop-to-mark-command )
(global-set-key (kbd "M-v" ) 'push-mark-command   )

;; List all buffers in current window instead of other one
(global-set-key "\C-x\C-b" 'buffer-menu) ; default is list-buffers


;; -------------------------------------------------------------------
;; Swap right/left buffers with C-tab
;; -------------------------------------------------------------------

(let ((file "~/.emacs.d/buffer-move.el"))
  (when (file-readable-p file)
    (load-file file)

    (defun rotate-buffers (&rest args)
      "Switch left and right buffers"
      (interactive)
      (if (ignore-errors (buf-move-right) t) (other-window 1) (buf-move-left)))

    (global-set-key (kbd "C-<tab>") 'rotate-buffers)

    ))


;; -------------------------------------------------------
;; Kill current and other buffers
;; -------------------------------------------------------

(defun kill-other-buffer nil
  "Kill buffer in next window"
  (interactive)
  (kill-buffer (window-buffer (next-window))))

(global-set-key (kbd "C-x k"   ) 'kill-current-buffer )
(global-set-key (kbd "C-x 4 k" ) 'kill-other-buffer   )


;; -------------------------------------------------------------------
;; Backward kill line
;; Same C-u as terminal
;;
;; Fetched on 12/08/2019 from:
;; https://www.emacswiki.org/emacs/BackwardKillLine
;; -------------------------------------------------------------------

(defun backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (if (= 1 arg) (if (bolp) (kill-line -1) (kill-line 0))
    (kill-line (- arg))))

(global-set-key (kbd "C-u") 'backward-kill-line)


;; -------------------------------------------------------
;; Sexp management
;; -------------------------------------------------------

(defun forward-copy-sexp nil
  (interactive)
  (mark-sexp)
  ;(kill-ring-save nil nil t)
  (copy-region-as-kill nil nil t)
  )

(global-set-key (kbd "C-("             ) 'raise-sexp         )
(global-set-key (kbd "C-M-w"           ) 'forward-copy-sexp  )
(global-set-key (kbd "<C-M-backspace>" ) 'backward-kill-sexp )
(global-set-key (kbd "<C-M-return>"    ) 'newline            )


;; -------------------------------------------------------
;; Highlight tabs, spaces and whitespaces in red
;; -------------------------------------------------------

(defface user-extra-whitespaces-face
  '((default
     :background "darkred"
     :underline  "red"
     ;:box (:line-width -1 :color "red")
     ))
  "Face of tabs and extra whitespaces"
  :group 'user-faces
  )

(add-hook 'prog-mode-hook ;'after-change-major-mode-hook
          (lambda nil
            (let ((keywords '(("\t"            . 'user-extra-whitespaces-face)
                              ("[[:space:]]+$" . 'user-extra-whitespaces-face)
                              )))
              (font-lock-remove-keywords nil keywords)
              (font-lock-add-keywords    nil keywords)
              )))

;; -------------------------------------------------------
;; Python settings
;; -------------------------------------------------------.

(add-hook 'python-mode-hook
          (lambda nil
            (set (make-local-variable 'tab-width) 2)
            (set (make-local-variable 'comment-start) "#")
            ))

(setq-default python-indent-offset 2)
(setq-default sh-basic-offset      2)
(setq-default perl-indent-level    2)

;; -------------------------------------------------------
;; Indent S-expressions
;; -------------------------------------------------------

(defun custom-indent-sexp ()
  "Aligns current sexp key arguments using regular expression"
  (interactive)
  (save-mark-and-excursion
    (backward-up-list)
    (mark-sexp)
    (align-regexp (region-beginning) (region-end) "?\\w+\\(\\s-+\\)")
    ))

(global-set-key (kbd "C-M-<tab>") 'custom-indent-sexp)

;; -------------------------------------------------------
;; Align columns
;; -------------------------------------------------------

(defun align-columns ()
  "Align columns in region"
  (interactive)
  (when (region-active-p)
    (call-process-region (region-beginning) (region-end) "~/.emacs.d/bin/column" t t t)
    ))

;; Make sure emacs does not split horizontally
(setq split-height-threshold nil)

;; -------------------------------------------------------
;; Split window only twice
;; -------------------------------------------------------

(setq split-width-threshold (- (window-width) 10))
(setq split-height-threshold nil)

(defun count-visible-buffers (&optional frame)
  "Count how many buffers are currently being shown. Defaults to selected frame."
  (length (mapcar #'window-buffer (window-list frame))))

(defun do-not-split-more-than-two-windows (window &optional horizontal)
  (if (and horizontal (> (count-visible-buffers) 1))
      nil
    t))

(advice-add 'window-splittable-p :before-while #'do-not-split-more-than-two-windows)


