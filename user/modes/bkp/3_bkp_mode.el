;Simple mode to write Lisp code
;Author : Aurelien Buchet

;; =================================================================================
;; Opening .el files in LISP mode automatically
;; =================================================================================

(add-to-list 'auto-mode-alist '("\\.bkp\\'" . bkp-mode))

;; =================================================================================
;; Changing fonts depending on the text
;; =================================================================================

(defvar bkp-font-lock
  (list
    ;; Order is important ! First it matchs functions then methods then classes etc
    ;; macros
    ;(cons (concat "\\<" (regexp-opt bkp-macros t) "\\>") ''user-form-face); two ' because emacs-basic-faces are associated to a symbol returning their name [(eval font-lock-variable-name-face) returns font-lock-variable-name-face]
    ;; functions
    ;(cons (concat "\\<" (regexp-opt bkp-functions t) "\\>") ''user-function-face)
    ;; forms
    ;(cons (concat "\\<" (regexp-opt bkp-forms t) "\\>") ''user-class-face)
    ;; symbols
    (cons "^\*.*\n" ''user-comment-face)
    ;; variables
    (cons "[\.][a-zA-Z_\-]*\\>" ''user-variable-face)
    ;; keywords (and prefixes)
    (cons (concat "\\<" (regexp-opt '("LABEL" "UNIT" "SWEEP" "TYPE") t) "\\>") ''user-keyword-face)
  );list
  "Default highlighting expressions for BKP mode"
);defvar

;; =================================================================================
;; Defining BKP Comment Style
;; =================================================================================

(defvar bkp-syntax-table
  (let ((bkp-syntax-table (make-syntax-table)))
    ;; line comment begins with ; and finish with newline
    (modify-syntax-entry ?\; "< b" bkp-syntax-table)
    (modify-syntax-entry ?\n "> b" bkp-syntax-table)
    bkp-syntax-table
  )
  "Defining BKP syntax table"
)

;; =================================================================================
;; Creating BKP Mode
;; =================================================================================

(defvar bkp-hooks nil)

(defun bkp-mode ()
  (interactive)
  ;; deleting local variables
  (kill-all-local-variables)
  ;; using BKP shortcuts (./shortcuts.el)
  (use-local-map bkp-map)
  ;; using BKP syntax for comments
  (set-syntax-table bkp-syntax-table)
  ;; using BKP fonts
  (set (make-local-variable 'font-lock-string-face) 'user-string-face)
  (set (make-local-variable 'font-lock-comment-face) 'user-symbol-face)
  (set (make-local-variable 'font-lock-defaults) '(bkp-font-lock))
  ;; using BKP indentation function
  (set (make-local-variable 'indent-line-function) 'bkp-indent-line)
  ;; allowing outline mode with BKP
  (set (make-local-variable 'outline-regexp) "^\\([a-zA-Z_\-]*(\\|/\\*\\|;.*\n\\)");to define headings (outline-minor-mode)
  ;; making BKP Mode
  (setq major-mode 'bkp-mode)
  (setq mode-name "BKP")
  (run-mode-hooks 'bkp-hooks)
);set

(provide 'bkp-mode)
