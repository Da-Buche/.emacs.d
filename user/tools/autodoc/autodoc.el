;; =============================================================================================================
;; @file autodoc.el
;; 
;; @doc This File contains all the functions and scripts in order to generate the latex template and the pdf from a file
;; 
;; @author Aurélien BUCHET
;; =============================================================================================================

;; =============================================================================================================
;; Reading lines, comments and tags
;; =============================================================================================================

;; -------------------------------------------------------------------------------------------------------------
;; @fun ADreadLines
;; 
;; @doc Return a list of lines of the file described by filePath
;; 
;; @arg filePath
;; @type string
;; @doc String containing the path of the file
;; 
;; @out 
;; @type list 
;; -------------------------------------------------------------------------------------------------------------
(defun ADreadLines (filePath)
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" nil)))

;; -------------------------------------------------------------------------------------------------------------
;; @fun ADcommentp
;; 
;; @doc Return t if object is a string describing a comment
;; 
;; @arg object
;; @type any
;; @doc Object to check
;; 
;; @out
;; @type boolean
;; -------------------------------------------------------------------------------------------------------------
;; Need to change this function so it can be used with any language
(defun ADcommentp (object)
  (and (stringp object) (> (length object) 0) (equal (substring object 0 1) ";")))

;; -------------------------------------------------------------------------------------------------------------
;; @fun ADreadCommentLines
;;
;; @doc Retunrs the list of comment lines of the file described by filePath
;; 
;; @arg filePath
;; @type string
;; @doc String containing the path of the file
;; 
;; @out 
;; @type list 
;; -------------------------------------------------------------------------------------------------------------
(defun ADreadCommentLines (filePath)
  (remove nil (mapcar (lambda (line) (when (ADcommentp line) line)) (ADreadLines filePath))))

;; -------------------------------------------------------------------------------------------------------------
;; @fun ADemptyCommentp
;;
;; @doc Return t if the given string describes an empty comment line
;;
;; @arg line
;; @type string
;; @doc The string to check wether it is an empty comment
;;
;; @out 
;; @type boolean
;; @doc t if the string describes an empty comment, nil otherwise
;; -------------------------------------------------------------------------------------------------------------
(defun ADemptyCommentp (line) (string-match "^;+\s*$" line))

;; -------------------------------------------------------------------------------------------------------------
;; @fun ADseparatorCommentp
;;
;; @doc Return t if the givent string describes a separator comment line
;; (A separator comment line starts with ;; and is composed of spaces then only - or =)
;; -------------------------------------------------------------------------------------------------------------
(defun ADseparatorCommentp (line) (string-match "^;+ *[=-]+$" line))


;; -------------------------------------------------------------------------------------------------------------
;; @fun ADextractComment
;;
;; @doc Retuns the substring placed after the comment marker in the given string
;; -------------------------------------------------------------------------------------------------------------
(defun ADextractComment (line)
  (let ((comment ""))
    (dolist (word (split-string line "[; ]" t))
      (setq comment (concat comment word " ")))
    (ADescapeString comment));let
);defun

;; -------------------------------------------------------------------------------------------------------------
;; @fun ADescapeUnderscores
;;
;; @doc place \_ instead of _ in a string
;; -------------------------------------------------------------------------------------------------------------
(defun ADescapeUnderscores (str)
  (let ((res ""))
    (dolist (sbstr (split-string str "_")) (setq res (concat res "\\_" sbstr)))
    (substring res 2));let
);defun

;; -------------------------------------------------------------------------------------------------------------
;; @fun ADescapeDollars
;;
;; @doc place \$ instead of $ in a string
;; -------------------------------------------------------------------------------------------------------------
(defun ADescapeDollars (str)
  (let ((res ""))
    (dolist (sbstr (split-string str "\\$")) (setq res (concat res "\\$" sbstr)))
    (substring res 2));let
);defun

;; -------------------------------------------------------------------------------------------------------------
;; @fun ADescapeString
;;
;; @doc Escape all the special characters in a string
;; -------------------------------------------------------------------------------------------------------------
(defun ADescapeString (str)
  (ADescapeDollars (ADescapeUnderscores str)))

;; -------------------------------------------------------------------------------------------------------------
;; @fun ADgetTag
;;
;; @doc Returns the tag of a line if it contains one, nil otherwise
;;
;; @arg line
;; @type string
;; @doc The string of a line in a file
;;
;; @out tag
;; @type string/nil
;; @doc The fetched tag or nil if any was found
;; -------------------------------------------------------------------------------------------------------------
(defun ADgetTag (line)
  (when (string-match-p " @.*" line)
    (car (split-string line "[; @]+" t))))

;; -------------------------------------------------------------------------------------------------------------
;; @fun ADgetTagValue
;;
;; @doc Retuns the substring placed after the given tag in the given string
;; -------------------------------------------------------------------------------------------------------------
(defun ADgetTagValue (line tag) 
  (ADescapeString (substring line (+ (string-match-p (concat "@" tag) line) (length tag) 1))))

;; -------------------------------------------------------------------------------------------------------------
;; @fun ADnextTag
;;
;; @doc Browse comment lines until the next tag, returns nil if all lines are browsed without fetching any tag
;; -------------------------------------------------------------------------------------------------------------
(defun ADnextTag (lines)
  (let (line tag)
    (while (and (not tag) (setq line (pop lines)))
      (setq tag (ADgetTag line)))
    ;; Returning the tag and the remaining lines
    (when tag (list tag (cons line lines)))
  );let
);defun

;; -------------------------------------------------------------------------------------------------------------
;; @fun ADgetCurrentTag
;;
;; @doc This function is used from a line containing a tag to get all of its content
;; (stops on a new tag, a separator or a non comment line)
;; -------------------------------------------------------------------------------------------------------------
(defun ADgetCurrentTag (lines)
  ;; This function is used from a line containing a tag to get all of its content
  ;; stops on a new tag, a separator or a non comment line
  (let (tag value
      (line (pop lines))
      newTag)
    ;; tag and value initialisation
    (setq tag (ADgetTag line))
    (setq value (ADgetTagValue line tag))
    ;; Browsing lines until the next tag, separator or non comment line
    (while (and (setq line (pop lines))
        (ADcommentp line)
        (not (ADseparatorCommentp line))
        (not (setq newTag (ADgetTag line))))
      (setq value (concat value "\n" (ADextractComment line)))
    );while
    ;; Removing '\n' and ' ' at the beginning of value
    (while (and (> (length value) 0) (member (substring value 0 1) '(" " "\n"))) (setq value (substring value 1)))
    ;; Removing '\n' at the end of value
    (while (and (> (length value) 0) (equal (substring value (- (length value) 1)) "\n"))
      (setq value (substring value 0 (- (length value) 1))))
    (list tag value newTag (when line (cons line lines)))
  );let
);defun

;; =============================================================================================================
;; Constructing property lists
;; =============================================================================================================

(defun ADconstructArgumentDPL (lines)
  ;; Returns the DPL of an argument header and the remaining lines after it
  (let (res tag value newTag DPL)
    ;; Getting current tag which should be @arg
    (setq res (ADgetCurrentTag lines)
      tag    (pop res)
      value  (pop res)
      newTag (pop res)
      lines  (pop res))
    ;; Checking that current tag is @arg
    (unless (equal tag "arg") (error "ADconstructArgumentDPL: used while current tag is not @arg"))
    (plist-push DPL "name" value)
    ;; Checking if a new tag was fetched and if its an output tag
    (while (and newTag (member newTag '("type" "kind" "doc" "def")))
      ;; new tag is ok putting its value in the DPL
      (setq res (ADgetCurrentTag lines)
        tag    (pop res)
        value  (pop res)
        newTag (pop res)
        lines  (pop res))
      (plist-push DPL tag value)
    );while
    ;; Returning the DPL
    (list DPL newTag lines)
  );let
);defun

(defun ADconstructOutputDPL (lines)
  ;; Returns the DPL of an output header and the remaining lines after it
  (let (res tag value newTag DPL)
    ;; Getting current tag which should be @out
    (setq res (ADgetCurrentTag lines)
      tag    (pop res)
      value  (pop res)
      newTag (pop res)
      lines  (pop res))
    ;; Checking that current tag is @arg
    (unless (equal tag "out") (error "ADconstructOutputDPL: used while current tag is not @out"))
    (plist-push DPL "name" value)
    ;; Checking if a new tag was fetched and if it is an output tag
    (while (and newTag (member newTag '("type" "doc")))
      ;; new tag is ok putting its value in the DPL
      (setq res (ADgetCurrentTag lines)
        tag    (pop res)
        value  (pop res)
        newTag (pop res)
        lines  (pop res))
      (plist-push DPL tag value)
    );while
    ;; Returning the DPL
    (list DPL newTag lines)
  );let
);defun

(defun ADconstructFunctionDPL (lines)
  ;; Returns the DPL of a function header and the remaining lines after it
  (let (arguments outputs subDPL
      res tag value newTag DPL)
    ;; Getting current tag which should be @fun
    (setq res (ADgetCurrentTag lines)
      tag    (pop res)
      value  (pop res)
      newTag (pop res)
      lines  (pop res))
    ;; Checking that current tag is @fun
    (unless (equal tag "fun") (error "ADconstructFunctionDPL: used while current tag is not @fun"))
    (plist-push DPL "name" value)
    ;; Checking if a new tag was fetched and if it is a function tag
    (while (and newTag (member newTag '("doc" "example" "arg" "out")))
      (case (symcat newTag)
        ;; new tag is an argument constructing its DPL and placing it in arguments list
        ('arg (setq res (ADconstructArgumentDPL lines)
            subDPL (pop res)
            newTag (pop res)
            lines  (pop res))
          (push subDPL arguments))
        ;; new tag is an output constructing its DPL and placing it in outputs list
        ('out (setq res (ADconstructOutputDPL lines)
            subDPL (pop res)
            newTag (pop res)
            lines  (pop res))
          (push subDPL outputs))
        ;; new tag is a classic tag putting its value in the DPL
        (t (setq res (ADgetCurrentTag lines)
            tag    (pop res)
            value  (pop res)
            newTag (pop res)
            lines  (pop res))
          (plist-push DPL tag value))
      );case newTag
    );while
    ;; Pushing arguments and outputs in the generated DPL
    (plist-push DPL 'arguments arguments)
    (plist-push DPL 'outputs outputs)
    ;; Returning the DPL
    (list DPL lines)
  );let
);defun

(defun ADconstructMacroDPL (lines)
  ;; Returns the DPL of a macro header and the remaining lines after it
  (let (arguments outputs subDPL
      res tag value newTag DPL)
    ;; Getting current tag which should be @macro
    (setq res (ADgetCurrentTag lines)
      tag    (pop res)
      value  (pop res)
      newTag (pop res)
      lines  (pop res))
    ;; Checking that current tag is @macro
    (unless (equal tag "macro") (error "ADconstructMacroDPL: used while current tag is not @macro"))
    (plist-push DPL "name" value)
    ;; Checking if a new tag was fetched and if it is a macro tag
    (while (and newTag (member newTag '("doc" "example" "arg" "out")))
      (case (symcat newTag)
        ;; new tag is an argument constructing its DPL and placing it in arguments list
        ('arg (setq res (ADconstructArgumentDPL lines)
            subDPL (pop res)
            newTag (pop res)
            lines  (pop res))
          (push subDPL arguments))
        ;; new tag is an output constructing its DPL and placing it in outputs list
        ('out (setq res (ADconstructOutputDPL lines)
            subDPL (pop res)
            newTag (pop res)
            lines  (pop res))
          (push subDPL outputs))
        ;; new tag is a classic tag putting its value in the DPL
        (t (setq res (ADgetCurrentTag lines)
            tag    (pop res)
            value  (pop res)
            newTag (pop res)
            lines  (pop res))
          (plist-push DPL tag value))
      );case newTag
    );while
    ;; Pushing arguments and outputs in the generated DPL
    (plist-push DPL 'arguments arguments)
    (plist-push DPL 'outputs outputs)
    ;; Returning the DPL
    (list DPL lines)
  );let
);defun

(defun ADconstructMethodDPL (lines)
  ;; Returns the DPL of a method header and the remaining lines after it
  (let (arguments outputs subDPL
      res tag value newTag DPL)
    ;; Getting current tag which should be @method
    (setq res (ADgetCurrentTag lines)
      tag    (pop res)
      value  (pop res)
      newTag (pop res)
      lines  (pop res))
    ;; Checking that current tag is @method
    (unless (equal tag "method") (error "ADconstructMethodDPL: used while current tag is not @method"))
    (plist-push DPL "name" value)
    ;; Checking if a new tag was fetched and if it is a method tag
    (while (and newTag (member newTag '("doc" "example" "arg" "out")))
      (case (symcat newTag)
        ;; new tag is an argument constructing its DPL and placing it in arguments list
        ('arg (setq res (ADconstructArgumentDPL lines)
            subDPL (pop res)
            newTag (pop res)
            lines  (pop res))
          (push subDPL arguments))
        ;; new tag is an output constructing its DPL and placing it in outputs list
        ('out (setq res (ADconstructOutputDPL lines)
            subDPL (pop res)
            newTag (pop res)
            lines  (pop res))
          (push subDPL outputs))
        ;; new tag is a classic tag putting its value in the DPL
        (t (setq res (ADgetCurrentTag lines)
            tag    (pop res)
            value  (pop res)
            newTag (pop res)
            lines  (pop res))
          (plist-push DPL tag value))
      );case newTag
    );while
    ;; Pushing arguments and outputs in the generated DPL
    (plist-push DPL 'arguments arguments)
    (plist-push DPL 'outputs outputs)
    ;; Returning the DPL
    (list DPL lines)
  );let
);defun

(defun ADconstructFieldDPL (lines)
  ;; Returns the DPL of an field header and the remaining lines after it
  (let (res tag value newTag DPL)
    ;; Getting current tag which should be @field
    (setq res (ADgetCurrentTag lines)
      tag    (pop res)
      value  (pop res)
      newTag (pop res)
      lines  (pop res))
    ;; Checking that current tag is @field
    (unless (equal tag "field") (error "ADconstructFieldDPL: used while current tag is not @field"))
    (plist-push DPL "name" value)
    ;; Checking if a new tag was fetched and if its an output tag
    (while (and newTag (member newTag '("type" "doc")))
      ;; new tag is ok putting its value in the DPL
      (setq res (ADgetCurrentTag lines)
        tag    (pop res)
        value  (pop res)
        newTag (pop res)
        lines  (pop res))
      (plist-push DPL tag value)
    );while
    ;; Returning the DPL
    (list DPL newTag lines)
  );let
);defun

(defun ADconstructClassDPL (lines)
  ;; Returns the DPL of a class header and the remaining lines after it
  (let (fields subDPL
      res tag value newTag DPL)
    ;; Getting current tag which should be @class
    (setq res (ADgetCurrentTag lines)
      tag    (pop res)
      value  (pop res)
      newTag (pop res)
      lines  (pop res))
    ;; Checking that current tag is @class
    (unless (equal tag "class") (error "ADconstructClassDPL: used while current tag is not @class"))
    (plist-push DPL "name" value)
    ;; Checking if a new tag was fetched and if it is a class tag
    (while (and newTag (member newTag '("doc" "field")))
      ;; Case on newTag
      (case (symcat newTag)
        ;; Field
        ('field (setq res (ADconstructFieldDPL lines)
            subDPL (pop res)
            newTag (pop res)
            lines  (pop res))
          (push subDPL fields))
        ;; new tag is a classic tag putting its value in the DPL
        (t (setq res (ADgetCurrentTag lines)
            tag    (pop res)
            value  (pop res)
            newTag (pop res)
            lines  (pop res))
          (plist-push DPL tag value))
      );case newTag
    );while
    ;; Pushing fields and outputs in the generated DPL
    (plist-push DPL 'fields  fields)
    ;; Returning the DPL
    (list DPL lines)
  );let
);defun

(defun ADconstructFileDPL (filePath)
  (let ((lines (ADreadCommentLines filePath))
      functions macros classes methods
      res tag value newTag 
      subDPL DPL)
    ;; Going to the nextTag
    (setq res (ADnextTag lines)
      newTag (pop res)
      lines  (pop res))
    ;; Checking if a new tag was fetched and if it is a file tag
    (while (and newTag (member newTag '("file" "doc" "author" "main" "fun" "macro" "class" "method")))
      ;; Case on newTag
      (case (symcat newTag)
        ;; fileName
        ('file (setq res (ADgetCurrentTag lines)
            tag    (pop res)
            value  (pop res)
            newTag (pop res)
            lines (pop res))
          (plist-push DPL 'name value))
        ;; Function
        ('fun (setq res (ADconstructFunctionDPL lines)
            subDPL (pop res)
            lines  (pop res))          
          (push subDPL functions))
        ;; Macro
        ('macro (setq res (ADconstructMacroDPL lines)
            subDPL (pop res)
            lines  (pop res))
          (push subDPL macros))
        ;; Class
        ('class (setq res (ADconstructClassDPL lines)
            subDPL (pop res)
            lines  (pop res))
          (push subDPL classes))
        ;; Method
        ('method (setq res (ADconstructMethodDPL lines)
            subDPL (pop res)
            lines  (pop res))
          (push subDPL methods))
        ;; new tag is a classic tag putting its value in the DPL
        (t (setq res (ADgetCurrentTag lines)
            tag    (pop res)
            value  (pop res)
            newTag (pop res)
            lines  (pop res))
          (plist-push DPL tag value))
      );case newTag
      ;; Fetching next tag
      (setq res (ADnextTag lines)
        newTag (pop res)
        lines  (pop res))
    );while
    ;; Pushing functions, macros, classes and methods in the generated DPL
    (plist-push DPL 'functions (nreverse functions))
    (plist-push DPL 'macros    (nreverse macros))
    (plist-push DPL 'classes   (nreverse classes))
    (plist-push DPL 'methods   (nreverse methods))
    ;; Returning the DPL
    DPL
  );let
);defun

(defun ADconstructProjectDPL (filePath)
  (let ((lines (ADreadCommentLines filePath))
      res tag value newTag DPL
    files)
    ;; Going to the nextTag
    (setq res (ADnextTag lines)
      newTag (pop res)
      lines  (pop res))
    ;; Checking if a new tag was fetched
    (while newTag
      (setq globalLines lines)

      (setq res (ADgetCurrentTag lines)
        tag    (pop res)
        value  (pop res)
        newTag (pop res)
        lines  (pop res))
      (plist-push DPL (if (equal tag "project") "name" tag) value)
      ;; Fetching next tag
      (setq res (ADnextTag lines)
        newTag (pop res)
        lines (pop res))
    );while
    ;; Browsing files to generate subDPLs
    (dolist (filePath (split-string (plist-val DPL 'files) "[ \n]" t))
      (push (ADconstructFileDPL filePath) files))
    (plist-push DPL 'files files)
  );let
);defun

;; ============================================================================================================
;; Browsing DPL to generate/fill tex file
;; ============================================================================================================

(defun ADreplace (tag value)
  ;; Finds the tag in current buffer and replaces it with the given value
  (save-excursion
    (beginning-of-buffer)
    (when (search-forward-regexp (strcat "@" tag) nil t)
      (replace-match value t t))))

(defun ADargumentDPLtoTexString (argument)
  (let (
      (kind (plist-val argument 'kind))
      (def  (plist-val argument 'def))
      (doc  (plist-val argument 'doc))
      (type (plist-val argument 'type))
    )
    (strcat "\n\\vspace{3mm}\n"
      ;; Argument name
      "\\begin{minipage}[t]{.25\\linewidth}\n"
      " {\\ttfamily " (if kind (strcat "@" kind " ") "") (if (equal kind "key") "?" "") (plist-val argument 'name) " }\n"
      "\\end{minipage}\n"
      ;; Argument docstring, type and default value on the right
      "\\begin{minipage}[t]{.75\\linewidth}\n"
      (if doc doc "") "\\vspace{1mm}\n"
      (if type (strcat "\\\\ -~type:    \\hspace{14mm plus 1cm minus 1cm} {\\ttfamily " type "}\\hfill\n") "")
      (if def  (strcat "\\\\ -~default: \\hspace{1cm plus 1cm minus 1cm} {\\ttfamily " def "}\\hfill\n") "")
      "\\end{minipage}\n\n"
    );strcat
  );let
);defun

(defun ADfunctionDPLtoTexString (fun)
  (let ((arguments (plist-val fun 'arguments))
        (outputs   (plist-val fun 'outputs))
        (example   (plist-val fun 'example)))
    (strcat
      ;; Function name and docstring
      "\\function{" (plist-val fun 'name) "}{\n\n"
      ;"\\begin{verbatim}\n\n "
      (plist-val fun 'doc) "\n\n"
      ;"\\end{verbatim}\n\n"
      ;; Function arguments
      (if arguments
        (strcat "\n\\vspace{3mm plus 2mm minus 2mm}\n{\\bfseries Arguments}\n\n"
          (apply 'strcat (mapcar 'ADargumentDPLtoTexString arguments)))
        "")
      ;; Function outputs
      (if outputs
        (strcat "\n\\vspace{3mm plus 2mm minus 2mm}\n{\\bfseries Returned values}\n\n"
          (apply 'strcat (mapcar 'ADargumentDPLtoTexString outputs)))
        "")
      ;; Example
      (if example
        (strcat "\n\\vspace{3mm plus 2mm minus 2mm}\n{\\bfseries Example}\n"
          "\\begin{verbatim}\n\n"
          example "\n\n"
          "\\end{verbatim}\n\n")
        "")
      "}\n\n"
    );strcat
  );let
);defun

(defun ADfileDPLtoTexString (file)
  (let (
      (doc       (plist-val file 'doc))
      (functions (plist-val file 'functions))
      (macros    (plist-val file 'macros))
      (classes   (plist-val file 'classes))
      (methods   (plist-val file 'methods))
    );let definitions
    (strcat "\\file{" (plist-val file 'name) "}{\n\n"
      ;; Docstring
      (if doc (strcat "{\\large " doc " }" "\\\\[3mm]\n\n") "")
      ;; Printing functions
      (if functions 
        (strcat "{\\bfseries \\LARGE Functions}\n\n"
          (apply 'strcat (mapcar 'ADfunctionDPLtoTexString functions)))
        "")
      "}\n\n");strcat
  );let
);defun

;; -------------------------------------------------------------------------------------------------------------
;; @fun ADfillTemplate
;;
;; @doc Take a latex template and fill the tags using the DPL of a project
;; -------------------------------------------------------------------------------------------------------------
(defun ADfillTemplate (projectPath templatePath outputPath)
  (let ((DPL (ADconstructProjectDPL projectPath))
      
    )
    ;; Copying the template into the output
    (with-temp-file outputPath
      (insert-file-contents-literally templatePath)
      ;; Replacing project name
      (ADreplace "project" (plist-val DPL 'name))
      ;; Replacing project values
      (dolist (tag '("acronym" "logo" "summary" "abstract" "date" "authors" "contact"))
        (ADreplace tag (plist-val DPL tag)))
      ;; Constructing the Tex string describing all the files and functions they contains
      (ADreplace "files" (apply 'strcat (mapcar 'ADfileDPLtoTexString (plist-val DPL 'files))))
    );with-temp-file
  );let
);defun

;; =============================================================================================================
;; Obsolete
;; =============================================================================================================


(defun ADgenerateTemplate (directory file plist)
  "Generates a file.tex in directory using the property list"
  (let
    ;; fetching project parameters
    (buffer
      ;; strings
      (project       (plist-val plist 'project 'name))
      (date          (plist-val plist 'project 'date))
      (logo          (plist-val plist 'project 'logo)) titlepagelogo 
      (summary       (plist-val plist 'project 'summary))
      (authors       (plist-val plist 'project 'authors))
      (contact       (plist-val plist 'project 'contact))
      (abstract      (plist-val plist 'project 'abstract))
      (readme        (plist-val plist 'project 'readme))
      (prerequisites (plist-val plist 'project 'prerequisites))
      (installation  (plist-val plist 'project 'installation))
      ;; files
      (files         (plist-val plist 'project 'files))
    )
    ;; checking entries
    ;; project, date, logo, summary, authors, contact, abstract
    (dolist (symbol '(project date logo summary authors contact abstract readme prerequisites installation))
      (eval `(check-string ,symbol)))
    (when (equal project "") (warn "project name is empty"))
    ;; logo (also checks if logo file exists and is readable)
    (when (file-readable-p logo)
      (setq titlepagelogo (concat "\\includegraphics[height=8cm]{" logo "}"))
      (setq logo (concat "\\includegraphics[height=2cm]{" logo "}")))
    ;; opening tex file and erasing it
    (when (setq buffer (find-file (concat directory "/" file ".tex")))
      (erase-buffer)
      ;; inserting tex code
      (insert 
        ;; setup packages
        "\\documentclass[a4paper, 11pt]{article}\n"
        "\\usepackage[utf8]{inputenc}\n"
        "\\usepackage[english]{babel}\n"
        "\\usepackage[labelformat=empty]{caption}\n"
        "\\usepackage{geometry}\n"
        "\\usepackage{graphicx}\n"
        "\\usepackage{float}\n"
        "\\usepackage{fancyhdr}\n"
        "\\usepackage{lastpage}\n"
        "\\usepackage{titlesec}\n"
        "\\usepackage{hyperref}\n"
        "\\usepackage{minted}\n\n"
        ;; hypersetup
        "\\hypersetup{\n"
        " colorlinks=true, urlcolor=black, linkcolor=black,\n"
        " breaklinks=true, %permet le retour à la ligne dans les liens trop longs\n"
        " bookmarksopen=false, %si les signets Acrobat sont créés,\n"
        " % les afficher complètement.\n"
        " pdftitle={" project "}, %informations apparaissant dans\n"
        " pdfauthor={Autodoc generated document}, %les informations du document\n"
        " pdfsubject={Documentation} %sous Acrobat.\n"
        "}\n\n"
        ;; section, subsection and subsubsection spacing
        "\\titlespacing\\section{0mm}{8mm}{0mm}"
        "\\titlespacing\\subsection{4mm}{-8mm}{0mm}"
        "\\titlespacing\\subsubsection{8mm}{-12mm}{0mm}"
        ;; geometry, headers and footers setup
        "\\geometry{hmargin=15mm,vmargin=25mm}\n"
        "\\setlength{\\topmargin}{-30pt}\n"
        "\\setlength{\\parindent}{0em}\n"
        "\\setlength{\\parskip}{5mm}\n"
        "\\pagestyle{fancy}\n"
        "\\renewcommand{\\headrulewidth}{1pt}\n"
        "\\lhead{" project "}\n"
        "\\chead{" date "} \n"
        "\\rhead{" logo "}\n"
        "\\renewcommand{\\footrulewidth}{1pt}\n"
        "\\lfoot{\\leftmark}\n"
        "\\cfoot{}\n"
        "\\rfoot{\\thepage\\ / \\pageref{LastPage}}\n\n"
        ;; creating sections without numbers
        "\\newcommand{\\nonumsection}[1]{\n"
        "\\newpage\\paragraph{}"
        "\\phantomsection\n"
        "\\addcontentsline{toc}{section}{#1}\n"
        "\\markboth{\\uppercase{#1}}{}\n"
        "\\section*{#1}}\n\n"
        ;; creating subsections without numbers
        "\\newcommand{\\nonumsubsection}[1]{\n"
        "\\paragraph{}"
        "\\phantomsubsection\n"
        "\\addcontentsline{toc}{subsection}{#1}\n"
        "\\subsection*{#1}}\n\n"
        ;; creating subsubsections without numbers
        "\\newcommand{\\nonumsubsubsection}[1]{\n"
        "\\paragraph{}"
        "\\phantomsubsubsection\n"
        "\\addcontentsline{toc}{subsubsection}{#1}\n"
        "\\subsubsection*{#1}}\n\n"
        ;; creating \file
        "\\newcommand{\\file}[2]{\n"
        "\\nonumsubsection{#1}"
        "#2}\n\n"
        ;; beginning of document
        "\\begin{document}\n"
        ;; inserting titlepage
        "\\begin{titlepage}\n"
        "\\begin{center}\n"
        titlepagelogo "\\\\[1cm]\n"
        "{\\huge \\bfseries " project "}\\\\[8mm]\n"
        "{\\large " date "}\\\\[16mm]\n"
        "\\begin{flushleft} \\large " summary " \\end{flushleft}\\\\[-5mm]\n"
        "\\rule{\\linewidth}{1pt}\\\\[5mm]\n"
        "\\begin{flushright} \\Large\n"
        authors "\\\\[3mm]\n\\large " contact 
        "\\end{flushright}\\\\[2cm]\n"
        "\\end{center}\n"
        abstract "\n"
        "\\vfill\n"
        "\\end{titlepage}\n\n"
        ;; table of contents (paragraph to place contents title properly)
        "\\newpage\\paragraph{}\\tableofcontents\\phantomsection\n"
        "\\markboth{CONTENTS}{}\\newpage\n\n"
        ;; printing readme, prerequisites and installation when they exist
        (if (equal readme "") "" (concat
            "\\nonumsection{Readme}\n" readme "\n\n"))
        (if (equal prerequisites "") ""
          (concat "\\nonumsection{Prerequisites}\n" prerequisites "\n\n"))
        (if (equal installation "") "" 
          (concat "\\nonumsection{Installation}\n" installation "\n\n"))
        (if (not files) ""
          (concat "\\nonumsection{Files}\n" (DPLfilesToTexString files) "\n"))
        ;; end of document
        "\\end{document}\n")
      ;; saving buffer and killing it
      (save-buffer buffer)
      (kill-buffer buffer)
    );when
  );let
);defun

(defun generatePDF (directory file plist)
  "Generates a pdf in directory using the property list"
  ;; generating .tex template
  (generateTemplate directory file plist)
  ;; generating PDF
  (let ((command (format
          "CURRENTDIR=\"$(pwd)\" && cd %s ; pdflatex -interaction nonstopmode -shell-escape %s.tex ; pdflatex -interaction nonstopmode -shell-escape %s.tex ; rm %s.aux %s.log %s.out %s.toc %s.pyg ; cd $CURRENTDIR"
          directory file file file file file file file)))
  (shell-command command)
  command))

;(generatePDF "/home/aurelien/git/.emacs.d.git/trunk/user/tools/autodoc" "test" DPL)

(defun printPDF (directory file)
  "Print the pdf file using a shell command"
  ;; Move to directory, print PDF using pdflatex 2 times for references then removes generated files and come back to initial directory
  (let ((command
         (format
          "CURRENTDIR=\"$(pwd)\" && cd %s && pdflatex -interaction nonstopmode -shell-escape %s && pdflatex -interaction nonstopmode -shell-escape %s ; cd $CURRENTDIR"
          directory
          (concat file ".tex") (concat file ".tex")
          file file file file)))
    (shell-command command)
    command))





