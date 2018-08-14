;Simple mode to write Lisp code
;Author : Aurelien Buchet

;; =================================================================================
;; Opening .el files in LISP mode automatically
;; =================================================================================

(add-to-list 'auto-mode-alist '("\\.el\\'" . lisp-mode))

;; =================================================================================
;; Changing fonts depending on the text
;; =================================================================================

(defvar lisp-font-lock
  (list
    ;; Order is important ! First it matchs functions then methods then classes etc
    ;; macros
    (cons (concat "\\<" (regexp-opt lisp-macros t) "\\>") ''user-form-face); two ' because emacs-basic-faces are associated to a symbol returning their name [(eval font-lock-variable-name-face) returns font-lock-variable-name-face]
    ;; functions
    (cons (concat "\\<" (regexp-opt lisp-functions t) "\\>") ''user-function-face)
    ;; forms
    (cons (concat "\\<" (regexp-opt lisp-forms t) "\\>") ''user-class-face)
    ;; symbols
    (cons "\'[a-zA-Z_\-]*\\>" ''user-symbol-face)
    ;; variables
    (cons "[&\?][a-zA-Z_\-]*\\>" ''user-variable-face)
    ;; keywords (and prefixes)
    (cons (concat "\\<" (regexp-opt '("t" "nil" "()") t) "\\>") ''user-keyword-face)
  );list
  "Default highlighting expressions for LISP mode"
);defvar

;; =================================================================================
;; Defining LISP Comment Style
;; =================================================================================

(defvar lisp-syntax-table
  (let ((lisp-syntax-table (make-syntax-table)))
    ;; line comment begins with ; and finish with newline
    (modify-syntax-entry ?\; "< b" lisp-syntax-table)
    (modify-syntax-entry ?\n "> b" lisp-syntax-table)
    lisp-syntax-table
  )
  "Defining LISP syntax table"
)

;; =================================================================================
;; Creating LISP Mode
;; =================================================================================

(defvar lisp-hooks nil)

(defun lisp-mode ()
  (interactive)
  ;; deleting local variables
  (kill-all-local-variables)
  ;; using LISP shortcuts (./shortcuts.el)
  (use-local-map lisp-map)
  ;; using LISP syntax for comments
  (set-syntax-table lisp-syntax-table)
  ;; using LISP fonts
  (set (make-local-variable 'font-lock-string-face) 'user-string-face)
  (set (make-local-variable 'font-lock-comment-face) 'user-comment-face)
  (set (make-local-variable 'font-lock-defaults) '(lisp-font-lock))
  ;; using LISP indentation function
  (set (make-local-variable 'indent-line-function) 'lisp-indent-line)
  ;; allowing outline mode with LISP
  (set (make-local-variable 'outline-regexp) "^\\([a-zA-Z_\-]*(\\|/\\*\\|;.*\n\\)");to define headings (outline-minor-mode)
  ;; making LISP Mode
  (setq major-mode 'lisp-mode)
  (setq mode-name "LISP")
  (run-mode-hooks 'lisp-hooks)
);set

(provide 'lisp-mode)
