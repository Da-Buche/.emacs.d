;; ============================================================================================================
;; skill-mode.el
;;
;; Major mode for editing Cadence SKILL and SKILL++
;; (This mode is greatly inspired by lisp-interaction-mode)
;;
;; Author: Aurelien Buchet - aurelien.buchet@tutanota.com
;;
;; 2017 - 2021
;; ============================================================================================================

(require 'cl)
(require 'cl-lib)
(require 'cl-macs)
(require 'subr-x)

;; =======================================================
;; Custom Faces
;; =======================================================

;; -------------------------------------------------------
;; The fonts and faces are generated using the following
;; list of associations, each type of highlighted word has
;; a created face using defface called skill-<type>-face
;; -------------------------------------------------------

(let (;; the following list describes each element that can be recognized in SKILL code
      ;; and the associated color in which emacs will highlight it
      (fonts '(
               ;; ------------------------------------------------------
               ;; Fonts defined by a list of strings
               ;; name, color and slant
               ;; ------------------------------------------------------
               ("class"        "firebrick")
               ("info"         "dark orange"      "italic")
               ("comment"      "DarkGoldenrod")
               ("form"         "light steel blue")
               ("function"     "Deep sky blue")
               ("keyword"      "plum")
               ("method"       "indian red")
               ("shortcut"     "chartreuse")
               ("string"       "dark salmon")
               ("symbol"       "rosy brown")
               ("syntax-form"  "light goldenrod")
               ("variable"     "medium sea green")
               ))
      type color slant)
  ;; Create each font defined above
  (dolist (font fonts) (setq type (pop font)) (setq color (pop font)) (setq slant (pop font))
          (unless slant (setq slant "normal")) (setq slant (intern slant))
          (eval `(defface ,(intern (concat "skill-" type "-face")) '((default :foreground ,color :slant ,slant))
                   ,(concat "Face of " type "s") :group 'skill-faces))
          ));dolist ;let

;; =======================================================
;; Utils
;; =======================================================

(defun skill-make-string (sym-or-str)
  "Return SYM-OR-STR as a string"
  (format "%s" sym-or-str))

(defun skill-beginning-of-sexp nil
  "Move to the beginning of current sexp. Return the number of nested sexp the point was over or after. [from elisp-mode]"
  (let ((parse-sexp-ignore-comments t)
        (num-skipped-sexps 0))
    (condition-case _
        (progn
          ;; First account for the case the point is directly over a
          ;; beginning of a nested sexp.
          (condition-case _
              (let ((p (point)))
                (forward-sexp -1)
                (forward-sexp 1)
                (when (< (point) p)
                  (setq num-skipped-sexps 1)))
            (error))
          (while
              (let ((p (point)))
                (forward-sexp -1)
                (when (< (point) p)
                  (setq num-skipped-sexps (1+ num-skipped-sexps))))))
      (error))
    num-skipped-sexps))

(defun skill-fnsym-in-current-sexp nil
  "Return a list of current function name and argument index."
  (save-excursion
    (unless (nth 8 (syntax-ppss))
      (let ((argument-index (1- (skill-beginning-of-sexp))))
        ;; If we are at the beginning of function name, this will be -1.
        (when (< argument-index 0)
          (setq argument-index 0))
        (list (current-word) argument-index)))))

;; =======================================================
;; Global Variables
;; =======================================================

;; Here we only add the syntax forms that are not properly defined in Cadence SKILL
;; documentation (in .fnd files)
(defvar skill-syntax-forms '(and or defglobalfun) "List of skill syntax forms")

;; =======================================================
;; Eldoc Support
;; =======================================================

(defvar skill-eldoc-functions-table (make-hash-table :test #'equal)
  "List containing SKILL functions docstrings, load file and associated line etc.")

(defvar skill-eldoc-variables-table (make-hash-table :test #'equal)
  "List containing SKILL variables docstrings, load file and associated line etc.")

(defun skill-eldoc-shape-argstring (str name)
  "Parse .fnd argument STR, beautify it and return it"
  ;; Remove newlines
  (setq str (replace-regexp-in-string "\n[[:space:]]*" " " str))
  ;; Create one line by expression (ending with =>)
  (setq str (replace-regexp-in-string
                   "\\(=>[[:space:]]*[A-z0-9_]+\\([[:space:]]*/[[:space:]]*[A-z0-9_]+\\)*\\)"
                   "\\1\n"
                   str))
  ;; Shape each expression
  (let ((hl-name  (concat "" name))
        (hl-arrow "=>")
        )
    (add-text-properties 0 (length hl-name ) '(face skill-function-face) hl-name )
    (add-text-properties 0 (length hl-arrow) '(face skill-shortcut-face) hl-arrow)
    (setq str (mapconcat (lambda (str)
                           (setq str (replace-regexp-in-string "^[[:space:]]*"           ""    str))
                           (setq str (replace-regexp-in-string "[[:space:]]+"            " "   str))
                           ;; Highlight name
                           (setq str (replace-regexp-in-string (concat "^\\(.*?\\)(?" name "(?") (concat "\\1" hl-name ": (") str))
                           ;; Highlight arrow
                           (setq str (replace-regexp-in-string "=>" hl-arrow str))
                           )
                         (split-string str "\n") "\n")))
  ;; Return shaped string
  str)

(defun skill-eldoc-fill-functions-table (&optional directories)
  "Parse .fnd files to fill `skill-eldoc-variables-table'"
  ;; Parse $CDS_FINDER_PATH to fetch .fnd files
  (unless directories
    (setq directories (or (split-string (or (getenv "CDS_FINDER_PATH") "") ":" t)
                          ;; Path can be obtained by (simplifyFilename (strcat (car (getInstallPath)) "/../../doc/finder/"))
                          '("~/.skill/finder/")
                          )))
  (dolist (dir directories)
    (dolist (fnd-file (or (ignore-errors (directory-files-recursively dir ".fnd$" nil t t))
                                         (directory-files-recursively dir ".fnd$" nil)
                          ))
      ;; Read each sexp from .fnd file
      (with-temp-buffer
        (insert-file-contents fnd-file)
        (goto-char (point-min))
        (let (sexp)
          (with-demoted-errors "Reading .fnd File"
            (while (setq sexp (read (current-buffer)))
              (cl-destructuring-bind (name argstring &optional docstring &rest args) sexp
                ;; Support several function names passed together
                (dolist (name (split-string name "[, ]" t))
                  ;; Make sure function name is valid

                  ;; Parse docstring to check function data
                  (when (string-match "this is a syntax form" docstring)
                    (cl-pushnew name skill-syntax-forms)
                    )
                  ;; Shape argstring
                  (setq argstring (skill-eldoc-shape-argstring argstring name))
                  ;; Builld property list
                  (setf (gethash name skill-eldoc-functions-table)
                        `(argstring ,argstring docstring ,docstring fnd-file ,fnd-file))
                  ))
              )))
        ))
    ))

(defun skill-get-fnsym-args-string (sym &optional index prefix)
  "Return a string containing the parameter list of the function SYM.
If SYM is a subr and no arglist is obtainable from the docstring
or elsewhere, return a 1-line docstring."
  (let* ((plist     (gethash (format "%s" sym) skill-eldoc-functions-table))
         (docstring (plist-get plist 'docstring))
         (argstring (plist-get plist 'argstring))
         )
    (if docstring (concat argstring "\n" docstring) argstring)
    ))

(defun skill-get-var-docstring (sym)
  "Return a string containing a brief (one-line) documentation string for SYM variable [from elisp-mode]"
  ;; Not defined yet
  nil)

(defun skill-eldoc-documentation-function nil
  "`eldoc-documentation-function' (which see) for Cadence SKILL."
  (let ((current-symbol (current-word))
        (current-fnsym  (skill-fnsym-in-current-sexp))
        )
    (cond
     ;; Not defining a function, do not show any doc
     ((null current-fnsym)
      nil)
     ;; Current symbol is the function one, fetch its documentation
     ((eq current-symbol (car current-fnsym))
      (or (apply #'skill-get-fnsym-args-string current-fnsym)
          (skill-get-var-docstring current-symbol)))
     ;; Otherwise, check if current symbol is known
     (t
      (or (skill-get-var-docstring current-symbol)
          (apply #'skill-get-fnsym-args-string current-fnsym))
      )
     )))

;; =======================================================
;; Font Lock Support
;; =======================================================

(defun skill-font-lock-function-matcher (search-limit)
  "Search next function name from point to SEARCH-LIMIT, if it
succeeds return a non-nil value, move point and set match-data"
  (let (name new-point)
    (save-excursion
      ;; Browse words until a matching one is found or search-limit is exceeded
      (while (and (right-word)
                  (< (point) search-limit)
                  (setq name (current-word))
                  (not (and (gethash name skill-eldoc-functions-table)
                            ;; A matching one was found, update match-data
                            (left-word)
                            (re-search-forward name search-limit)
                            ;; Store new point to move it afterward
                            (setq new-point (point))
                            )))
        ;; Do nothing outside the looping condition
        nil))
    ;; Move point and return t when matching name was found
    (when new-point
      (goto-char new-point)
      t)
    ));let ;def

(defvar skill-font-lock-keywords nil "Words highlighting for SKILL mode")
(defun skill-update-font-lock-keywords nil
  "Make sure `skill-font-lock-keywords' are up-to-date"
  (setq skill-font-lock-keywords
        `(
          ;; Comments starting with ;
          (";;.*$" 0 'skill-info-face t)
          ;; t, nil or ()
          ("\\(()\\|\\<\\(\\(nil\\|t\\)\\>\\)\\)" . 'skill-keyword-face)
          ;; SKILL Identifiers used as prefixes
          ;; (cf. Cadence SKILL Language Reference -> Identifiers Used to Denote Data Types)
          ("\\(\\<[aAbBCdefFgGhIKlLmMnopqrRsStTuUvwxY]\\)_" . (1 'skill-keyword-face))
          ;; @<symbol> or ?<symbol>
          ("[@\?]\\w+\\>" . 'skill-variable-face)
          ;; C-style characters
          (,(regexp-opt '("'" "`" "," "=" ":=" ":" "<" ">" "+" "-" "*" "/"  "->" "~>" "&" "|")) . 'skill-shortcut-face)
          ;; Quoted symbols
          ("[\'`]\\w+\\>" . 'skill-symbol-face)
          ;; Functions being defined
          ("(def\\(globalf\\)?un\\([[:space:]]\\|\n\\)+\\(\\w+\\)" . (3 'skill-function-face))
          ;; Syntax forms
          (,(regexp-opt (mapcar 'skill-make-string skill-syntax-forms) 'words) . 'skill-syntax-form-face)
          ;; Functions, macros, classes and methods

          ;; Functions defined in documentation
          (skill-font-lock-function-matcher . 'skill-function-face)
          )))

;; =======================================================
;; SKILL Server Interaction
;; =======================================================

(defun skill-tcp-client-script nil
  "Return the value of $SKILL_TCP_CLIENT_SCRIPT"
  (cl-some
   (lambda (elt)
     (and (stringp elt)
          (setq elt (expand-file-name elt))
          (file-executable-p elt)
          elt))
   (list (getenv "SKILL_TCP_CLIENT_SCRIPT")
         (concat (getenv "HOME") "/.emacs.d/TCP/skill_tcp_client")
         )
   ))

(defun skill-eval-string (str)
  "Evaluate STR in SKILL using $SKILL_TCP_CLIENT_SCRIPT"
  (let ( ( script (skill-tcp-client-script) )
         )
    (cond
     ;; Check SKILL TCP server is available
     ( (not (stringp           script)) (message "$SKILL_TCP_CLIENT_SCRIPT is undefined") )
     ( (not (file-executable-p script)) (message "Unable to execute %s" script)           )
     ( (not (stringp           str   )) (message "Not a valid string: %S" str)            )
     ( t
       (string-trim
        (shell-command-to-string
         (format "echo '%s' | %s" (replace-regexp-in-string "'" "'\"'\"'" str) script)
         ))
       ));t ;cond
     ));let ;fun

(defun skill-message-eval-string (str)
  "Evaluate STR and message it to minibuffer, this is meant to be used in interactive functions"
  (let* ((res   (skill-eval-string str))
         (lines (split-string res "\n"))
         )
    ;; Remove wrapping parentheses when necessary
    (setq res
          (mapconcat
           (lambda (line)
             (when (and (< 2 (length line)) (equal "(" (substring line 0 1)) (equal ")" (substring line -1)))
               (setq line (substring line 1 -1))
               ))
           lines "\n"))
    ;; Print resulting message
    (message "%s" res)))

(defun skill-get-region nil
  "Return current region or sexp before point"
  (if (region-active-p) (buffer-substring-no-properties (mark) (point))
     ;; Fetch previous sexp
     (backward-sexp)
     (let ((beg (point)))
       (forward-sexp)
       (buffer-substring-no-properties beg (point)))))

(defun skill-eval-region nil
  "Evaluate selected region in Virtuoso, if region is note active send sexp before point instead"
  (interactive)
  (skill-message-eval-string (skill-get-region)))

(defun skill-eval-command (&optional command)
  "Evaluate COMMAND in SKILL and message the result"
  (interactive "sSKILL Command: ")
  (skill-message-eval-string command))

(defvar skill-command nil "Registered skill command")

(defun skill-register-command (&optional command)
  "Register SKILL COMMAND as the current one "
  (interactive "sSKILL Command: ")
  (setq skill-command command))

(defun skill-eval-registered-command nil
  "Evaluate registered SKILL command and print the result"
  (interactive)
  (message "Evaluating SKILL Command: %S" skill-command)
  (skill-message-eval-string skill-command))

(defun skill-save-get-current-file-name nil
  "Save current buffer and return its file name"
  (save-buffer)
  (let ( ( path (buffer-file-name) )
         )
    (setq path (replace-regexp-in-string "/Users/aurel/projects" "/unix/eda" path))
    (setq path (replace-regexp-in-string "/Users/aurel/unix"     "/unix"     path))
    path
    ));let ;fun

(defun skill-load-file (&optional path)
  "Load SKILL file located at PATH in Virtuoso"
  (interactive "fFile to load: ")
  (skill-message-eval-string
   (format "(if (isCallable '@load) (@load \"%s\") (load \"%s\"))" path path)
   ))

(defun skill-load-current-file nil
  "Load current SKILL file in Virtuoso"
  (interactive)
  (skill-load-file (skill-save-get-current-file-name)))

(defun skill-lint-file (&optional path)
  "Use Lint on SKILL file located at PATH"
  (interactive "fFile to lint: ")
  ;; VAR13: argument _... does not appear to be referenced (ignored as begins with '_')
  ;; VAR16: declaration of variable ... supersedes previous declaration at line ...
  (skill-message-eval-string
    (format "(inSkill (let ((path \"%s\")) (if (isCallable '@lint) (@lint ?files (list path)) (sklint ?file path ?ignores '(VAR13 VAR16 MEMBER1 REP110 dbOpenCellViewByType)))))" path)
    ;?ignores '(VAR12 CASE6 CHK15)
    ))

(defun skill-lint-current-file nil
  "Lint current SKILL file in Virtuoso"
  (interactive)
  (skill-lint-file (skill-save-get-current-file-name)))

;; =======================================================
;; Mode core
;; =======================================================

;; -------------------------------------------------------
;; Syntax Table
;; -------------------------------------------------------

(defvar skill-syntax-table
  (let ((skill-syntax-table (make-syntax-table)))
    ;; Allow Underscores in words
    (modify-syntax-entry ?\_ "w"   skill-syntax-table)
    (modify-syntax-entry ?\? "w"   skill-syntax-table)
    (modify-syntax-entry ?@  "w"   skill-syntax-table)
    ;; Paragrah comments /* ... */
    (modify-syntax-entry ?/ ". 14" skill-syntax-table)
    (modify-syntax-entry ?* ". 23" skill-syntax-table)
    ;; Line comments     ; ... \n
    (modify-syntax-entry ?\; "< b" skill-syntax-table)
    (modify-syntax-entry ?\n "> b" skill-syntax-table)
    skill-syntax-table)
  "SKILL syntax table, used in `skill-mode'")

;; -------------------------------------------------------
;; Indentation
;; -------------------------------------------------------

(defun skill-get-line-number nil
  "Return the line number where point is located"
  (string-to-number (format-mode-line "%l")))

(defun skill-get-column-number nil
  "Return the column number where point is located"
  ;(string-to-number (format-mode-line "%c"))
  (let ((current-point (point))) (save-excursion (beginning-of-line) (- current-point (point))))
  )

(defun skill-get-line-indent nil
  "Return indentation (in number of spaces) at curent point"
  (let (start end (spaces 0) (parentheses 0))
    (save-excursion (setq end (point)) (beginning-of-line) (setq start (point)))
    ;; Counts the number of spaces at beginning of line
    (while (equal (char-after start) ?\s) (setq start (+ 1 start)) (setq spaces (+ 1 spaces)))
    ;; Counts the number of opened parentheses before end
    (while (<= start end) (setq start (+ 1 start))
      (when (equal (char-after start) ?\() (setq parentheses (+ 1 parentheses))))
    (+ spaces (* 2 parentheses))
    ));let ;defun

(defun skill-forward-sexp nil
  "Fetch and return next sexp"
  (let ((start (point))
        (end   (and (ignore-errors (forward-sexp) t) (point)))
        )
    ;; Go to end of next sexp
    (when end
      (if (< start end)
          ;; A sexp was found, return it without spaces
          (buffer-substring-no-properties (save-excursion (backward-sexp) (point)) end)
        ;; No sexp were found
        (goto-char start)
        nil))))

(defun skill-get-current-sexp (&rest args)
  "Retrieve data about the current sexp of the current point
@key point  @type boolean @doc Provided point to search current sexp at, defaults to '(point)'
@key parent @type boolean @doc If non-nil retrieve data about parent sexp instead of current one


@out fun    @type string  @doc Name of the main function in current-sexp
@out indent @type integer @doc Indentation level of current sexp
@out index  @type integer @doc Index of current arg in current sexp
@out line   @type integer @doc Line number of current sexp
@out column @type integer @doc Column number of current sexp"
  ;; Fetch point where function is called
  (let ((current-point  (or (plist-get args :point) (point)))
        (previous-point 0)
        )
    (save-excursion
      (goto-char current-point)
      (when (ignore-errors (backward-up-list (if (plist-get args :parent) 2 1)) t)
        (let ((indent (skill-get-line-indent))
              (column (skill-get-column-number))
              (line   (skill-get-line-number))
              (index  0)
              (fun (progn (forward-char) (skill-forward-sexp)))
              arg args
              )
          (when fun
            ;; Fetch args
            (while (and (< previous-point (setq previous-point (point)) current-point)
                        (push (skill-forward-sexp) args))
              (setq index (+ 1 index)))
            (setq arg (pop args))
            ;; Return current attributes list
            (list fun indent line column index arg (nreverse args))
            ))))))

(defun skill-get-parent-sexp nil
  "Retrieves data about the parent sexp of the current point"
  (skill-get-current-sexp :parent t))

(defun skill-indent-specform (count state indent-point normal-indent)
  "From `lisp-indent-specform' but modified to align specially indented arguments"
  (let ((containing-form-start (elt state 1))
        (i count)
        body-indent containing-form-column)
    ;; Move to the start of containing form, calculate indentation
    ;; to use for non-distinguished forms (> count), and move past the
    ;; function symbol.  lisp-indent-function guarantees that there is at
    ;; least one word or symbol character following open paren of containing
    ;; form.
    (goto-char containing-form-start)
    (setq containing-form-column (current-column))
    (setq body-indent (+ lisp-body-indent containing-form-column))
    (forward-char 1)
    (forward-sexp 1)
    ;; Now find the start of the last form.
    (parse-partial-sexp (point) indent-point 1 t)
    (while (and (< (point) indent-point)
                (condition-case ()
                    (progn
                      (setq count (1- count))
                      (forward-sexp 1)
                      (parse-partial-sexp (point) indent-point 1 t))
                  (error nil))))
    ;; Point is sitting on first character of last (or count) sexp.
    (if (> count 0)
        ;; Reverted to old lisp indentation behavior where specially indented characters are aligned
        (list normal-indent containing-form-start)
      ;; A non-distinguished form.  Use body-indent if there are no
      ;; distinguished forms and this is the first undistinguished form,
      ;; or if this is the first undistinguished form and the preceding
      ;; distinguished form has indentation at least as great as body-indent.
      (if (or (and (= i 0) (= count 0))
              (and (= count 0) (<= body-indent normal-indent)))
          body-indent
          normal-indent))))

(defun skill-indent-function (indent-point state)
  "From `lisp-indent-function' but modified to use skill-indent-function property first"
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be a symbol
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
                (progn (goto-char calculate-lisp-indent-last-sexp)
                       (beginning-of-line)
                       (parse-partial-sexp (point)
                                           calculate-lisp-indent-last-sexp 0 t)))
            ;; Indent under the list or under the first sexp on the same
            ;; line as calculate-lisp-indent-last-sexp.  Note that first
            ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (function-get (intern-soft function) 'skill-indent-function)
                         (function-get (intern-soft function) 'lisp-indent-function)
                         ;; Function is documented in SKILL indent it properly
                         (and (gethash function skill-eldoc-functions-table) 0)
                         (get          (intern-soft function) 'lisp-indent-hook)
                         ))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (skill-indent-specform method state indent-point normal-indent))
              (method
                (funcall method indent-point state)))))))

(defun skill-indent-foreach (point state)
  "Properly indent foreach forms (taking in account foreach <mapping_function>)"
  (cl-destructuring-bind ( paren-depth containing-list-start last-complete-sexp-start in-string in-comment quoted
                           min-paren-depth comment-style string-or-comment-start paren-starts two-char-construct-start
                           &rest args
                           ) state
    ;; Fetch previous args
    (let ((current-indent (progn (goto-char containing-list-start) (current-column)))
          previous-args current-sexp previous-args-count)
      (forward-char 1)
      (while (<= (point) last-complete-sexp-start)
        (when (setq current-sexp (skill-forward-sexp))
          (push current-sexp previous-args)
          ))
      (setq previous-args       (reverse previous-args))
      (setq previous-args-count (length  previous-args))
      (when (member (cadr previous-args) '("mapc" "mapcar" "mapcan" "map" "maplist" "mapcon"))
        ;; Special foreach has one more argument
        (setq previous-args-count (1- previous-args-count)))
      ;(message "count: %S args: %S" previous-args-count previous-args)
      (cl-case previous-args-count
        (0 nil)
        (1
         ;; Align as special argument
         (+ (* 2 lisp-body-indent) current-indent))
        ((1 2)
         ;; Align with previous arg
         (goto-char last-complete-sexp-start) (current-column))
        (t
         ;; Align as body
         (+ lisp-body-indent current-indent))
        ))))

(defun skill-indent-list (point state)
  "Taking care of indenting `list' in hiCreate...Layout functions ?items argument"
  (cl-destructuring-bind ( paren-depth containing-list-start last-complete-sexp-start in-string in-comment quoted
                           min-paren-depth comment-style string-or-comment-start paren-starts two-char-construct-start
                           &rest args
                           ) state
    (setq paren-starts (reverse paren-starts))
    (let ((parent-indent (cadr paren-starts)))
      (when parent-indent
        (let ((parent-function (progn (goto-char parent-indent) (forward-char 1) (skill-forward-sexp))))
          ;; When parent function is a form layout one, indent accordingly
          (when (member parent-function '("hiCreateFormLayout" "hiCreateVerticalBoxLayout" "hiCreateHorizontalBoxLayout"))
            (goto-char parent-indent)
            (+ (* 2 lisp-body-indent) (current-column))
            ))))
    ))

(defun lisp-ppss-report nil
  ""
  (interactive)
  (cl-destructuring-bind ( paren-depth containing-list-start last-complete-sexp-start in-string in-comment quoted
                           min-paren-depth comment-style string-or-comment-start paren-starts two-char-construct-start
                           &rest args
                           ) (lisp-ppss (point))
    (message "\
paren-depth: %S
containing-list-start: %S
last-complete-sexp-start: %S
in-string: %S
in-comment: %S
quoted: %S
min-paren-depth: %S
comment-style: %S
string-or-comment-start: %S
paren-starts: %S
two-char-construct-start: %S
args: %S"
             paren-depth containing-list-start last-complete-sexp-start in-string in-comment quoted
             min-paren-depth comment-style string-or-comment-start paren-starts two-char-construct-start
             args
             )))

;; Setting adapted indentation for specific functions and macros
;; See `lisp-indent-function' in `lisp-mode' documentation

(dolist (tuple '((foreach skill-indent-foreach)
                 (list    skill-indent-list)

                 (cond                0)

                 (letseq              1)
                 (letf                1)
                 (prog                1)
                 (define              1)
                 (defvar              1)
                 (set                 1)
                 (setq                1)
                 (setf                1)
                 (pushf               1)
                 (case                1)
                 (caseq               1)
                 (mapcar              1)
                 (assert              1)
                 (when                1)
                 (awhen               1)
                 (unless              1)
                 (funcall             1)
                 (apply               1)
                 (lsprintf            1)

                 (defglobalfun        2)
                 (defmethod           2)
                 (destructuringBind   2)
                 (setof               2)
                 (exists              2)
                 (forall              2)
                 (nif                 2)
                 (aif                 2)
                 (wrap                2)

                 (for    3)
                 (mapfor 3)

                 (hiDisplayForm                0)
                 (hiCreateLayoutForm           2)
                 (hiCreateMenu                 2)
                 (hiCreateSimpleMenu           2)
                 (hiCreateFormLayout           1)
                 (hiCreateVerticalBoxLayout    1)
                 (hiCreateHorizontalBoxLayout  1)
                 (hiAddField                   1)

                 (leCreateLayerField 1)
                 ))
  (cl-destructuring-bind (sym val) tuple
    (put sym 'skill-indent-function val)))

(defun skill-indent-line (&optional indent)
  "Indent current line as SKILL code"
  (interactive)
  (let ((pos (- (point-max) (point)))
        (indent (progn (beginning-of-line)
                       (or indent
                           (skill-calculate-indent (lisp-ppss))
                           )))
        )
    (skip-chars-forward " \t")
    (if (or (null indent) (and (looking-at "\\s<") (not (looking-at "\\s<\\s<"))))
        ;; Don't alter indentation of a single-semicolon comment line
        ;; or a line that starts in a string.
        (goto-char (- (point-max) pos))
      (if (looking-at "\\s<\\s<\\s<")
          ;; Comment lines starting with three semicolons should be indented as
          ;; comment lines, not as code.
          (progn (indent-for-comment) (forward-char -1))
        (if (listp indent) (setq indent (car indent)))
        (indent-line-to indent))
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
          (goto-char (- (point-max) pos))))))

; (defun skill-indent-region (start end)
;   "Indent region as Lisp code, efficiently."
;   (save-excursion
;     (setq end (copy-marker end))
;     (goto-char start)
;     (beginning-of-line)
;     ;; The default `indent-region-line-by-line' doesn't hold a running
;     ;; parse state, which forces each indent call to reparse from the
;     ;; beginning. That has O(n^2) complexity.
;     (let* ((parse-state (lisp-indent-initial-state))
;            (pr (unless (minibufferp)
;                  (make-progress-reporter "Indenting region..." (point) end))))
;       (let ((ppss (lisp-indent-state-ppss parse-state)))
;         (unless (or (and (bolp) (eolp)) (nth 3 ppss))
;           (skill-indent-line (skill-calculate-indent ppss))))
;       (let ((indent nil))
;         (while (progn (setq indent (lisp-indent-calc-next parse-state))
;                       (< (point) end))
;           (unless (or (and (bolp) (eolp)) (not indent))
;             (skill-indent-line indent))
;           (and pr (progress-reporter-update pr (point)))))
;       (and pr (progress-reporter-done pr))
;       (move-marker end nil))))

; (defun skill-calculate-indent nil
;   "Caclucate current line indent as SKILL code"
;   ;; Fetch current point then go to beginning of line
;   (let ((current-point (prog1 (point) (beginning-of-line)))
;         )
;     ;; Retrieve current-sexp data then parent-sexp data
;     (cl-destructuring-bind (&optional current-fun current-indent current-line current-column current-index current-arg current-args)
;                         (skill-get-current-sexp)
;       (cl-destructuring-bind (&optional parent-fun parent-indent parent-line parent-column parent-index parent-arg parent-args)
;                           (skill-get-parent-sexp)
;         nil
;         ))))

(defun skill-calculate-indent (&optional parse-start)
  "Return appropriate indentation for current line as Lisp code.
In usual case returns an integer: the column to indent to.
If the value is nil, that means don't change the indentation
because the line starts inside a string.

PARSE-START may be a buffer position to start parsing from, or a
parse state as returned by calling `parse-partial-sexp' up to the
beginning of the current line.

The value can also be a list of the form (COLUMN CONTAINING-SEXP-START).
This means that following lines at the same level of indentation
should not necessarily be indented the same as this line.
Then COLUMN is the column to indent to, and CONTAINING-SEXP-START
is the buffer position of the start of the containing expression."
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
          state
          ;; setting this to a number inhibits calling hook
          (desired-indent nil)
          (retry t)
          calculate-lisp-indent-last-sexp containing-sexp)
      (cond ((or (markerp parse-start) (integerp parse-start))
             (goto-char parse-start))
            ((null parse-start) (beginning-of-defun))
            (t (setq state parse-start)))
      (unless state
        ;; Find outermost containing sexp
        (while (< (point) indent-point)
          (setq state (parse-partial-sexp (point) indent-point 0))))
      ;; Find innermost containing sexp
      (while (and retry
                  state
                  (> (elt state 0) 0))
        (setq retry nil)
        (setq calculate-lisp-indent-last-sexp (elt state 2))
        (setq containing-sexp (elt state 1))
        ;; Position following last unclosed open.
        (goto-char (1+ containing-sexp))
        ;; Is there a complete sexp since then?
        (if (and calculate-lisp-indent-last-sexp
                 (> calculate-lisp-indent-last-sexp (point)))
            ;; Yes, but is there a containing sexp after that?
            (let ((peek (parse-partial-sexp calculate-lisp-indent-last-sexp
                                            indent-point 0)))
              (if (setq retry (car (cdr peek))) (setq state peek)))))
      (if retry
          nil
        ;; Innermost containing sexp found
        (goto-char (1+ containing-sexp))
        (if (not calculate-lisp-indent-last-sexp)
            ;; indent-point immediately follows open paren.
            ;; Don't call hook.
            (setq desired-indent (current-column))
          ;; Find the start of first element of containing sexp.
          (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
          (cond ((looking-at "\\s(")
                 ;; First element of containing sexp is a list.
                 ;; Indent under that list.
                 )
                ((> (save-excursion (forward-line 1) (point))
                    calculate-lisp-indent-last-sexp)
                 ;; This is the first line to start within the containing sexp.
                 ;; It's almost certainly a function call.
                 (if (= (point) calculate-lisp-indent-last-sexp)
                     ;; Containing sexp has nothing before this line
                     ;; except the first element.  Indent under that element.
                     nil
                   ;; Skip the first element, find start of second (the first
                   ;; argument of the function call) and indent under.
                   (progn (forward-sexp 1)
                          (parse-partial-sexp (point)
                                              calculate-lisp-indent-last-sexp
                                              0 t)))
                 (backward-prefix-chars))
                (t
                 ;; Indent beneath first sexp on same line as
                 ;; `calculate-lisp-indent-last-sexp'.  Again, it's
                 ;; almost certainly a function call.
                 (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point) calculate-lisp-indent-last-sexp
                                     0 t)
                 (backward-prefix-chars)))))
      ;; Point is at the point to indent under unless we are inside a string.
      ;; Call indentation hook except when overridden by lisp-indent-offset
      ;; or if the desired indentation has already been computed.
      (let ((normal-indent (current-column)))
        (cond ((elt state 3)
               ;; Inside a string, don't change indentation.
               nil)
              ((and (integerp lisp-indent-offset) containing-sexp)
               ;; Indent by constant offset
               (goto-char containing-sexp)
               (+ (current-column) lisp-indent-offset))
              ;; in this case calculate-lisp-indent-last-sexp is not nil
              (calculate-lisp-indent-last-sexp
               (or
                ;; try to align the parameters of a known function
                (and lisp-indent-function
                     (not retry)
                     (funcall lisp-indent-function indent-point state))
                ;; -------------------------------------------------------
                ;; Custom SKILL indentation
                ;; -------------------------------------------------------
                (cl-destructuring-bind ( paren-depth containing-list-start last-complete-sexp-start in-string in-comment quoted
                                         min-paren-depth comment-style string-or-comment-start paren-starts two-char-construct-start
                                         &rest args
                                         ) state
                  (let ((paren-start (car (last paren-starts))))
                    (or
                     ;; Point is in the first newline of a sexp
                     (when (and paren-start
                                (= (line-number-at-pos last-complete-sexp-start) (line-number-at-pos paren-start))
                                )
                       (goto-char paren-start) (forward-char 1)
                       (cond
                        ;; data list (indicated by a space after paren)
                        ((looking-at "[[:space:]]") (skip-chars-forward "[[:space:]]") (current-column))
                        ;; function
                        ;; 1- to compensate previous `forward-char'
                        ((looking-at "\\w") (1- (+ (current-column) lisp-body-indent)))
                        ))
                     )))
                ;; If the function has no special alignment
                ;; or it does not apply to this argument,
                ;; try to align a constant-symbol under the last
                ;; preceding constant symbol, if there is such one of
                ;; the last 2 preceding symbols, in the previous
                ;; uncommented line.
                (and (save-excursion
                       (goto-char indent-point)
                       (skip-chars-forward " \t")
                       (looking-at "?"))
                     ;; The last sexp may not be at the indentation
                     ;; where it begins, so find that one, instead.
                     (save-excursion
                       (goto-char calculate-lisp-indent-last-sexp)
                       ;; Handle prefix characters and whitespace
                       ;; following an open paren.  (Bug#1012)
                       (backward-prefix-chars)
                       (while (not (or (looking-back "^[ \t]*\\|([ \t]+"
                                                      (line-beginning-position))
                                       (and containing-sexp
                                            (>= (1+ containing-sexp) (point)))))
                         (forward-sexp -1)
                         (backward-prefix-chars))
                       (setq calculate-lisp-indent-last-sexp (point)))
                     (> calculate-lisp-indent-last-sexp
                        (save-excursion
                          (goto-char (+ 2 containing-sexp))
                          (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
                          (point)))
                     (let ((parse-sexp-ignore-comments t)
                           indent)
                       (goto-char calculate-lisp-indent-last-sexp)
                       (or (and (looking-at "?")
                                (setq indent (current-column)))
                           (and (< (line-beginning-position)
                                   (prog2 (backward-sexp) (point)))
                                (looking-at "?")
                                (setq indent (current-column))))
                       indent))
                ;; another symbols or constants not preceded by a constant
                ;; as defined above.
                normal-indent))
              ;; in this case calculate-lisp-indent-last-sexp is nil
              (desired-indent)
              (t
               normal-indent)
              ))
      )))

;; -------------------------------------------------------
;; Bindkeys
;; -------------------------------------------------------
(defvar skill-mode-map
  (let ((map (make-keymap)) pair key callback)
    (dolist (pair '(("C-x C-e" skill-eval-region             "Evaluate selected region"      )
                    ("C-x p"   skill-load-current-file       "Load current file in virtuoso" )
                    ("C-x l"   skill-lint-current-file       "Runs lint on current file"     )
                    ("M-z"     skill-eval-command            "Send skill command"            )
                    ("C-x C-z" skill-register-command        "Register command"              )
                    ("C-z"     skill-eval-registered-command "Send registered command"       )
                    ;; Inserts a closed parentheses or fetched the function
                    ;; associated to the paired parenthese with '\)'
                                        ;("C-M-\)" skill-close-parentheses)
                    ;; Inserts a quotation mark or quote multiple words by
                    ;; repeating '\"'
                                        ;("\"" skill-insert-quotation-marks)
                    ;; Newline and indent with 'return'
                                        ;("<return>" skill-newline-and-indent)



                    ;; Display Virtuoso SKILL log with 'C-x C-l'
                                        ;("C-x C-l" skill-clean-log)
                    ;; Evaluates current buffer with 'C-x return' or
                    ;; 'C-x C-return'
                                        ;("C-x RET" skill-eval-buffer)
                                        ;("C-x <C-return>" skill-eval-buffer)
                    ;; Traces and untraces variables or functions with
                    ;; 'C-x /' and 'C-x C-/'
                                        ;("C-x /" skill-trace)
                                        ;("C-x C-/" skill-untrace)
                    ;; Dimm comments with 'C-x d'
                                        ;("C-x d" skill-dimm-comments)
                    ;; Launches SKILL IDE on current file with 'C-x i'
                                        ;("C-x i" skill-launch-ide)
                    ))
      (cl-destructuring-bind (key callback &rest _args) pair
        (define-key map (kbd key) callback)))
    ;; Return defined keymap
    map)
  "Keymap for SKILL major mode")

;; -------------------------------------------------------
;; Actual mode definition
;; -------------------------------------------------------

(define-derived-mode skill-mode lisp-mode "SKILL"
  "Major mode for editing Cadence SKILL code"
  :group 'lisp
  ;; Setup eldoc
  (when (hash-table-empty-p skill-eldoc-functions-table) (skill-eldoc-fill-functions-table))
  (add-function :before-until (local 'eldoc-documentation-function)
                #'skill-eldoc-documentation-function)
  ;; Setup fonts
  (setq-local font-lock-string-face  'skill-string-face )
  (setq-local font-lock-comment-face 'skill-comment-face)
  ;; Setup font-lock
  (skill-update-font-lock-keywords)
  (setq-local font-lock-defaults (list skill-font-lock-keywords nil nil))
  ;; Update syntax table
  (set-syntax-table skill-syntax-table)
  ;; Properly set comments in `comment-region'
  (setq-local comment-add 0) ;default to ';' instead of ';;'
  ;; Update indentation
  (setq-local indent-line-function 'skill-indent-line)
  (setq-local lisp-indent-function 'skill-indent-function)
  ;(setq-local lisp-indent-offset   2)
  ;; Use bindkeys
  (use-local-map skill-mode-map)
  )

;; Opening SKILL/SKILL++ files in SKILL mode
(add-to-list 'auto-mode-alist `(,(regexp-opt '(".il" ".ils" ".skill" ".scm" ".cdsinit" ".pp" ".dumb")) . skill-mode))
(add-to-list 'auto-mode-alist `(,(regexp-opt '(".loader" ".cdsinit" ".cdslocal" ".cdsperso" ".algo" ".brick")) . skill-mode))

;; -------------------------------------------------------
;; Skeletons
;; -------------------------------------------------------

(define-skeleton skill-skeleton-closure
  "Place a SKILL++ closure"
  "Function name: "
  "(let ()" \n
  -2 \n
  "(defglobalfun " str | "function_name" " ( @key  (cv  (geGetEditCellView))" \n
                                            "      (win (geGetEditCellViewWindow cv))" \n
                                         -6 "@rest _args )" \n
    "\"" _ "\"" \n
    \n
    ");def" \n
    -2 \n
    ");closure"\n
    )

(define-skeleton skill-skeleton-defun
  "Place a SKILL++ function"
  "Function name: "
  "(defun " str | "function_name" " ()" \n
    "\"" _ "\"" \n
    \n
    ");def" \n
    )

(define-key insert-map "c" 'skill-skeleton-closure)
(define-key insert-map "d" 'skill-skeleton-defun  )

