
;; =================================================================================
;; Classes
;; =================================================================================

;; (defclass comment nil
;;   ((beginning :initarg :beg
;;       :initform "" :type string
;;       :documentation "Beginning of comment")
;;     (end :initarg :end
;;       :initform "" :type string
;;       :documentation "End of comment"))
;;   "Describes beginning and end of one comment type in a language")

;; (defclass markup nil
;;   ((beginning :initarg :beg
;;       :initform "" :type string
;;       :documentation "Beginning of markup")
;;     (end :initarg :end
;;       :initform "" :type string
;;       :documentation "End of markup")
;;     (isRegexp :initarg :regexpp
;;       :initform nil :type boolean
;;       :documentation)
;;   "Describes beginning and end of one markup type but also the list of properties it can handle")

;; (defclass language nil
;;   ;; @class @name comment
;;   ;; @slots
;;   ;; @
;;   ((name :initarg :name
;;       :initform "" :type string
;;       :documentation "The name of the language.")
;;     (comments :initarg :comments
;;       :initform nil :type list
;;       :documentation "Methods to write comment in the language.")
;;     (markups :initarg :markups
;;       :initform nil :type list
;;       :documentation "Balises to generate documentation")
;;     (extensions :initarg :extensions
;;       :initform nil :type list))
;;   "Describes a language and how to generate documentation from it")

;; =================================================================================
;; Creating example property list
;; =================================================================================

(setq function (list
    'name "printPDF"
    'arguments (list
      (list
        'name "directory"
        'type "string"
        'doc "name of the directory containing the tex file"
      );list directory
      (list 
        'name "file"
        'type "string"
        'doc "name of the tex file without .tex suffix"
      );list file
    );list arguments
    'outputs (list
      (list
        'name "command"
        'type "string"
        'doc "command sent to shell in order to generate pdf"
      )
    );list output
    'doc "printPDF takes a directory and a file in arguments and generates the pdf associated to file.tex in directory"
  );list printPDF
);setq

(setq functions (list
    function
    function
  );list
);setq

(setq file (list
    'name "autodoc.el" 
    'functions functions
    'doc "autodoc.el contains the whole autodoc script"
  );list
);setq

(setq DPL (list
    'project
    (list
      'name "autodoc"
      'date "September 2018"
      'authors "A. Buchet"
      'contact "au.buchet@gmail.com"
      'summary "An automatic documentation generator using emacs-lisp"
      'abstract "autodoc is an open source documentation generator using emacs-lisp to generate a .tex documentation file and can print it as a .pdf file using pdftex.\n autodoc is fully configurable and can be used with many languages and many templates."
      'prerequisites "autodoc requires emacs and pdftex with some packages installed:\\\\inputenc\\\\babel\\\\caption\\\\geometry\\\\fancyhdr\\\\lastpage\\\\minted\\\\hyperref\\\\float"
      'logo "/home/aurelien/Pictures/logo.jpg"
      'files (list
        file
        file
      )
    );list project
  );list DPL
);setq
