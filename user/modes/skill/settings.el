;; =================================================================================
;; Settings for Cadence SKILL mode in emacs
;; 
;; A. BUCHET
;; =================================================================================

;; =================================================================================
;; Root
;; =================================================================================

;; Defining SKILL code root:
(defvar skill-code-root
  ;; This variable should contain a path or a list of path.
  ;; If this variable describes correct paths: all the SKILL files contained at the
  ;; locations will be parsed recursively in order to fecth all the defined 
  ;; forms, functions, classes and methods
  '(
    "/home/buchetau/SKILL"
    "/home/buchetau/SKILL/autoMenu/skill/layout"
    "/home/buchetau/SKILL/autoMenu/skill/schematic"
    "/home/buchetau/SKILL/autoMenu/skill/symbol"
    "/home/buchetau/SKILL/autoMenu/skill/all"
    "/home/buchetau/SKILL/DSIC/trunk/skill"
    "/home/buchetau/SKILL/VCT/modules/small_tools/skill"
  )
  "Root of SKILL code used to fetch names to be highlighted in emacs"
)

;; Defining Cadence root:
(defvar skill-virtuoso-root "/sw/cadence/ic/06.17.715/lnx86"
  "Root of Cadence installation"
)

;; Defining SKILL Documentation root:
(defvar skill-doc-root "/sw/cadence/ic/06.17.715/lnx86/doc/finder/SKILL"
  "Root of SKILL documentation (at this path all the SKILL modules folders containing .fnd files should be located)"
)

;; Defining SKILL Documentation files:
(defvar skill-doc-files
  '(
    ;"/sw/cadence/ic/06.17.715/lnx86/doc/finder/SKILL/DFII_SKILL/skdfref.fnd"
  )
  "Paths of .fnd files"
)

;; Defining search method in user code:
(defvar skill-recursive-search nil
  "If non-nil functions, forms, methods and classes will be searched recursively from skill-code-root"
)

;; =================================================================================
;; Log
;; =================================================================================

(defvar skill-log-path "/home/buchetau/SKILL/skill.log"
  "Path of the SKILL log that can be displayed by Emacs should be the same as defined in emacsConnects.ils"
)

;; =================================================================================
;; Shortcuts
;; =================================================================================

(defvar skill-map
  (let (
	(skill-mode-map (make-keymap))
      )
    (define-key skill-mode-map (kbd "C-x r")          'skill-reload)  
    (define-key skill-mode-map "("                    'skill-insert-parentheses)
    (define-key skill-mode-map "\""                   'insert-quote-marks)
    (define-key skill-mode-map ")"                    'check-closed-parentheses)
    (define-key skill-mode-map (kbd "<return>")       'skill-newline-and-indent)
    (define-key skill-mode-map (kbd "M-;")            'skill-comment-region)
    (define-key skill-mode-map (kbd "C-;")            'skill-insert-title)
    (define-key skill-mode-map ";"                    'skill-insert-comment)
    (define-key skill-mode-map (kbd "<backspace>")    'skill-delete-backward)
    (define-key skill-mode-map (kbd "C-d")            'skill-delete-forward)
    (define-key skill-mode-map (kbd "C-x p")          'skill-load-file)
    (define-key skill-mode-map (kbd "C-x l")          'skill-lint-file)
    (define-key skill-mode-map (kbd "C-x C-l")        'skill-clean-log)
    (define-key skill-mode-map (kbd "C-x C-e")        'skill-eval-region)
    (define-key skill-mode-map (kbd "C-x RET")        'skill-eval-buffer)
    (define-key skill-mode-map (kbd "C-x <C-return>") 'skill-eval-buffer)
    (define-key skill-mode-map (kbd "C-x /")          'skill-trace)
    (define-key skill-mode-map (kbd "C-x C-/")        'skill-untrace)
    skill-mode-map
  )
  "Keymap for SKILL major mode"
);defvar

;; =================================================================================
;; Fonts and faces
;; =================================================================================

;; Fonts foreach type
(let
  (
    couple type color
    (color-associations
      ;; the following list describes each element that can be recognized in SKILL code
      ;; and the associated color in which emacs will highlight it
      '(
        ("class"        "firebrick")
        ("comment"      "dark orange")
        ("form"         "light steel blue")
        ("function"     "Deep sky blue")
        ("info"         "gold")
        ("keyword"      "plum")
        ("method"       "indian red")
        ("shortcut"     "chartreuse")
        ("string"       "dark salmon")
        ("symbol"       "rosy brown")
        ("syntax-form"  "light goldenrod")
        ("variable"     "medium sea green")
      )
  ))
  ;; creating the associated face foreach type
  (while (setq couple (pop color-associations))
    (setq type  (pop couple))
    (setq color (pop couple)) 
    ;; defining the face
    (eval
      `(defface ,(intern (concat "skill-" type "-face"))
        '((default :foreground ,color))
        ,(concat "Face of " type "s")
        :group 'skill-faces
      )
    )
  )
)

(defvar skill-modules-highlights
  ;; The functions defined in those modules will be fetched in documentation
  ;; The modules can be defined using only a string but also with a couple:
  ;; The couple should contain the module name and the list of prefixes of the functions to fetch
  '(
    ;"ADE_Assembler"
    ;"Custom_Constraints"
    ;"Netlisting_and_Simulation"
    ;"Schematics"
    ;"ADE-L/"
    ;"Custom_Layout"
    ;"OCEAN"
    ;"Tech_File"
    ;"ADE_Verifier"
    ("DFII_SKILL" . ("dbOpen" "geGet" "dbGet"))
    ;too many functions in this module for emacs
    ;"OCEAN_XL"
    ;"Text_Editor"
    ;"ADE_XL"
    ;"DIVA_SKILL"
    ;"Parasitic_Aware_Design"
    ;"Translators"
    ;"AMS_Environment"
    ;"DRACULA_GUI_SKILL"
    ;"Pcells"
    "User_Interface"
    ;"Analog_Expression"
    ;"EDIF200"
    ;"Power_IR_EM"
    ;"ViVA_SKILL"
    "Core_SKILL"
    ;"HDL_Import_and_Netlist_Conversion"
    ;"Relative_Object_Design"
    ;"Voltus-Fi-XL"
  )
  "List of modules in which functions will be fetched and highlighted when editing SKILL code"
)

;; =================================================================================
;; Variables
;; =================================================================================

(defvar skill-syntax-forms
  ;; The syntax forms are detailled here
  ;; Those functions have particular definitions and are part of SKILL core
  '(
    "declare" "defprop" "defvar" "getq" "getqq" "setf" "setq" "exists" "existss"
    "for" "fors" "forall" "foralls" "foreach" "foreachs" "if" "go" "setof" "setofs"
    "unless" "when" "while" "measureTime" "sstatus" "begin" "defdynamic" "define"
    "defun" "dynamic" "errset" "lambda" "let" "mprocedure" "nlambda" "nprocedure"
    "prog" "prog1" "prog2" "progn" "pop" "push"
  )
  "List of SKILL syntax forms"
)

(defvar skill-forms
  '("car" "cdr" "caar"
    "cadr" "cdar" "cddr"
    "caaar" "caadr" "cadar" "caddr" "cdaar" "cdadr" "cddar" "cddr"
    "member" "memq" "memv"
    "sh" "shell"
  )
  "List of SKILL forms"
)

(defvar skill-functions
  '()
  "List of SKILL functions"
)

(defvar skill-classes
  '()
  "List of SKILL classes"
)

(defvar skill-methods
  '()
  "Listo of SKILL methods"
)

(provide 'skill-settings)

