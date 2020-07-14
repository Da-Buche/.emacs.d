;; =============================================================================================================
;; charac-bkp-mode.el 
;; 
;; Major mode for editing Mentor charac-bkp
;; 
;; Author: Aurelien Buchet - aurelien.buchet@tutanota.com
;; 
;; 2019
;; =============================================================================================================

;; =============================================================================================================
;; Bindkeys
;; =============================================================================================================

;; -------------------------------------------------------------------------------------------------------------
;; @var charac-bkp-map
;;
;; @doc Keymap for CHARAC-BKP major mode
;; 
;; This keymap contains all the bindkeys making CHARAC-BKP code edition easy
;; -------------------------------------------------------------------------------------------------------------
(defvar charac-bkp-map
  (let ((keymap (make-keymap)) pair key callback)
    (dolist (pair '(
          ;; ------------------------------------------------------ 
          ;; Launch simulation using current file
          ;; ------------------------------------------------------
          ("C-x r" charac-bkp-mode)
          ;; ------------------------------------------------------
          ;; Start EZ-wave showing current wdb
          ;; ------------------------------------------------------
          )) (setq key (pop pair)) (setq callback (pop pair)) (define-key keymap (eval `(kbd ,key)) callback))
    ;; Returns the keymap
    keymap) "Keymap for CHARAC-BKP major mode");defvar

;; =============================================================================================================
;; Default opening
;; =============================================================================================================

(add-to-list 'auto-mode-alist `(,(regexp-opt '(".bkp" ".txt")) . charac-bkp-mode))

;; -------------------------------------------------------------------------------------------------------------
;; Syntax Table
;; -------------------------------------------------------------------------------------------------------------

(defvar charac-bkp-syntax-table
  (let ((syntax-table (make-syntax-table)))
    ;; Allow Underscores in words
    (modify-syntax-entry ?\_ "w"   syntax-table)
    ;; Line comments     ; ... \n
    (modify-syntax-entry ?!  "<" syntax-table)
    (modify-syntax-entry ?\n ">" syntax-table)
    syntax-table) "CHARAC-BKP syntax table")

;; -------------------------------------------------------------------------------------------------------------
;; Highlighted keywords
;; -------------------------------------------------------------------------------------------------------------

(defvar charac-bkp-commands (regexp-opt '(
      ".option" ".param" ".sigbus" ".dc" ".ac" ".tran" ".probe" ".probebus" ".extract")) 
  "Regexp matching commands in .bkp files")

(defvar charac-bkp-sources (concat "\\<" (regexp-opt '(
      "dc" "ac" "pulse" "pwl")) "\\>")
  "Regexp matching sources in .bkp files")

(defvar charac-bkp-font-lock-keywords nil "Keywords to be highlighted in CHARAC-BKP mode")

(defun charac-bkp-generate-keywords nil
  ;; Reset keywords
  (setq charac-bkp-font-lock-keywords nil)
  (dolist (keyword `(
        ;; Comments
        ("^\\*.*$" 0 'font-lock-comment-face t)
        ;; Sources
        (,charac-bkp-sources . 'font-lock-type-face)
        ;; Commands
        (,charac-bkp-commands . 'font-lock-function-name-face)
        ;; + to continue a line
        ("^\\+"               . 'font-lock-warning-face)
        ;; V to define a voltage
        ("^V"                 . 'font-lock-keyword-face)
        ;; ------------------------------------------------------
        ;; 2. nil, t or ()
        ;; ------------------------------------------------------
        ;("\\(()\\|\\<\\(\\(nil\\|t\\)\\>\\)\\)" . 'charac-bkp-keyword-face)
        ;("\\(\\<[aAbBCdefFgGhIKlLmMnopqrRsStTuUvwxY]\\)_" . (1 'charac-bkp-keyword-face))
        )) (push keyword charac-bkp-font-lock-keywords))
  ;; Return compiled keywords
  charac-bkp-font-lock-keywords)

;; -------------------------------------------------------------------------------------------------------------
;; Hooks
;; -------------------------------------------------------------------------------------------------------------

(defvar charac-bkp-hooks nil)

;; -------------------------------------------------------------------------------------------------------------
;; Provide CHARAC-BKP mode
;; -------------------------------------------------------------------------------------------------------------

(defun charac-bkp-mode () "Enable CHARAC-BKP major mode" (interactive)
  ;; Use only CHARAC-BKP mode local variables
  (kill-all-local-variables)
  ;; Shortcuts
  (use-local-map charac-bkp-map)
  ;; Syntax table
  (set-syntax-table charac-bkp-syntax-table)
  (set (make-local-variable 'comment-start) "*")
  (set (make-local-variable 'comment-end) "")
  ;; Keywords
  ;; (nil to consider strings and comments, t to ignore case)
  (charac-bkp-generate-keywords)
  (set (make-local-variable 'font-lock-defaults) (list charac-bkp-font-lock-keywords nil t))
  ;; Major mode name
  (setq major-mode 'charac-bkp-mode) (setq mode-name "CHARAC-BKP")
  ;; Hooks
  (run-mode-hooks 'charac-bkp-hooks))

(provide 'charac-bkp-mode)

;; =============================================================================================================
;; End of charack-bkp-mode.el
;; =============================================================================================================
