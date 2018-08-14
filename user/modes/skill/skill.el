;; =================================================================================
;; Mode to write Cadence SKILL/SKILL++ code
;; 
;; A. BUCHET
;; =================================================================================

(require 'skill-settings)
(require 'skill-fetch)
(require 'skill-functions)
(require 'skill-log)
(require 'button-lock)

;; =================================================================================
;; Opening .il and .ils file in SKILL mode automatically
;; =================================================================================

(add-to-list 'auto-mode-alist '("\\.il\\'" . skill-mode))
(add-to-list 'auto-mode-alist '("\\.ils\\'" . skill-mode))

;; =================================================================================
;; Changing fonts depending on the text
;; =================================================================================

(defun skill-associated-lock-couple (type)
  "Returns the couple used to define the font-lock"
  ;; type is function/form/class/method
  (let (
      string
      (face (intern (concat "skill-" (symbol-name type) "-face")))
    )
    ;; little trick to avoid error because class plural is classes
    (when (eq type 'class) (setq type 'classe))
    ;; creating a huge string containing all the function/form/class/method names separated by spaces
    (setq string (concat "\\<" (regexp-opt (eval (intern (concat "skill-" (symbol-name type) "s"))) t) "\\>"))
    ;; (two ' because emacs-basic-faces are associated to a symbol returning their name
    ;; [(eval font-lock-variable-name-face) returns font-lock-variable-name-face])
    (cons string `',face)
  );let
);defun

(defconst skill-font-lock-keywords-pointer
  '`(
    ;("\"\\(.*\n\\)*.*\"" . 'skill-string-face)
    (")\\(;.*\\)$" . (1 'skill-info-face))
    ("\\(/\\*\\(.*\n\\)*.*\\*/\\|;.*$\\)" . 'skill-comment-face)
    ,(skill-associated-lock-couple 'function)
    ,(skill-associated-lock-couple 'method)
    ,(skill-associated-lock-couple 'class)
    ,(skill-associated-lock-couple 'form)
    ,(skill-associated-lock-couple 'syntax-form)
    ("defun[\s\n]*\\([a-zA-Z0-9_]*\\)" . (1 'skill-function-face))
    ("\'[a-zA-Z_]*\\>" . 'skill-symbol-face)
    ("\\('\\|`\\|,\\|<\\|>\\|=\\|->\\|~>\\|&\\|+\\|-\\|*\\|/\\:\\)" . 'skill-shortcut-face)
    ("[@\?][a-zA-Z_]*\\>" . 'skill-variable-face)
    ("\\(()\\|\\<\\(\\(nil\\|t\\)\\>\\)\\)" . 'skill-keyword-face)
    ("\\(\\<[pwgdlxfrsot]\\)_" . (1 'skill-keyword-face))
  )
)

(defvar skill-font-lock-keywords
  (progn
    (skill-fetch-documentation)
    (skill-fetch-code)
    (eval skill-font-lock-keywords-pointer)
  )
  "Default highlighting expressions for SKILL mode"
);defvar

(defun skill-update-keywords nil
  "Update the list of highlighted functions, forms, classes and methods"
  (skill-fetch-documentation)
  (skill-fetch-code)
  (setq skill-font-lock-keywords (eval skill-font-lock-keywords-pointer))
)

;; =================================================================================
;; Defining SKILL Comment Style (replaced by comment definition in skill-font-lock)
;; =================================================================================

(defvar skill-syntax-table
  (let ((skill-syntax-table (make-syntax-table)))
    ;; line comment begins with ; and finish with newline
    ;(modify-syntax-entry ?\; "< b" skill-syntax-table)
    ;(modify-syntax-entry ?\n "> b" skill-syntax-table)
    ;; paragrah comment begins with /* and finishes with */
    (modify-syntax-entry ?/ ". 14" skill-syntax-table);1 for first beginning character, 4 for second ending character
    (modify-syntax-entry ?* ". 23" skill-syntax-table);2 for second beginning character, 3 for first ending character
    skill-syntax-table
  )
  "Defining SKILL syntax table"
)

;; =================================================================================
;; Creating SKILL Mode
;; =================================================================================

(defvar skill-hooks nil)

(defun skill-mode ()
  "Enable SKILL major mode"
  (interactive)
  ;; deleting local variables in buffer
  (kill-all-local-variables)
  ;; using SKILL shortcuts (./shortcuts.el)
  (use-local-map skill-map)
  ;; Increasing max number of functions that can be fetched
  (set (make-local-variable 'max-lisp-eval-depth) 10000)
  ;; using SKILL syntax for comments
  (set-syntax-table skill-syntax-table)
  ;; using SKILL fonts
  (set (make-local-variable 'font-lock-string-face) 'skill-string-face)
  (set (make-local-variable 'font-lock-comment-face) 'skill-comment-face)
  (set (make-local-variable 'font-lock-defaults) 
    (list 
      skill-font-lock-keywords;regexp to match functions, forms, classes...
      nil;don't want to ignore strings and comments
      nil;don't want to ignore case
    )
  )
  ;; using SKILL indentation function
  (set (make-local-variable 'indent-line-function) 'skill-indent-line)
  ;; allowing outline mode with SKILL
  (set (make-local-variable 'outline-regexp) "^\\([a-zA-Z_]*(\\|/\\*\\|;.*\n\\)");to define headings (outline-minor-mode)
  ;; making SKILL Mode
  (setq major-mode 'skill-mode)
  (setq mode-name "SKILL")
  (run-mode-hooks 'skill-hooks)
)

(defun skill-reload ()
  "Updates functions, forms, classes and methods then reloads SKILL major mode"
  (interactive)
  ;; updating user and documentation definitions
  (skill-update-keywords)
  ;; reloading skill-mode
  (skill-mode)
)

(provide 'skill-mode)



