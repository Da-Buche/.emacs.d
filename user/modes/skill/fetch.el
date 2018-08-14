;; =================================================================================
;; Script to fetch the recognizable SKILL elements in all the files from root
;; 
;; A. BUCHET
;; =================================================================================

;; ================================================================================= 
;; Fetching functions and forms in documentation if root is defined
;; =================================================================================

(defun skill-filter-list (names prefixes)
  "Returns the list of names matching prefixes"
  (let (res prefix prefixes-copy)
    (dolist (name names)
      (setq prefixes-copy prefixes)
      (while (setq prefix (pop prefixes-copy))
        (when (> (length name) (length prefix))
          (when (equal prefix (substring name 0 (length prefix)))
            (push name res)
            (setq prefixes-copy nil)
          )
        )
      )
    )
    res
  );let
);defun

(skill-filter-list '("abcdef" "abcegobjn" "aiegjh" "afbjewhr" "gergb") '("abc" "a"))

(defun skill-fetch-functions (file &rest prefixes)
  "Returns the list of functions name in file which describes the path of a .fnd file"
  ;; Fetch all the functions defined in documentation
  ;; module-root is a path containing .fnd files
  (get-buffer-create "*skill-file-content*")
  (switch-to-buffer "*skill-file-content*")
  ;; .fnd files contains string lists only, emacs can interpret it
  (insert-file-contents file)
  (beginning-of-buffer)
  (insert "(quote (\n")
  (end-of-buffer)
  (insert "\n)\n)")
  (prog1
    (if prefixes
      (skill-filter-list (mapcar 'car (eval-last-sexp "*skill-file-content*")) prefixes)
      (mapcar 'car (eval-last-sexp "*skill-file-content*"))
    )
    ;; killing the created buffer
    (kill-buffer "*skill-file-content*")
  )
);defun 
(length (skill-fetch-functions "/sw/cadence/ic/06.17.715/lnx86/doc/finder/SKILL/DFII_SKILL/skdfref.fnd"))
(length (skill-fetch-functions "/sw/cadence/ic/06.17.715/lnx86/doc/finder/SKILL/DFII_SKILL/caiskill.fnd"))

(defun skill-fetch-documentation nil
  ;; fetching SKILL documentation directory
  (unless (and (boundp 'skill-doc-root) (stringp skill-doc-root) (file-directory-p skill-doc-root))
    ;; when doc root is missing checking if virtuoso root exists in order to find documentation here
    (when (and (boundp 'skill-virtuoso-root) (stringp skill-virtuoso-root))
      (setq skill-doc-root "/doc/finder/SKILL")
      (let (
          (skill-virtuoso-root skill-virtuoso-root)
          (split-root)
        )
        ;; fetching documentation root
        (while (and 
            (not (file-directory-p (concat skill-virtuoso-root skill-doc-root)))
            (not (eq skill-virtuoso-root "")))
          ;; going up one directory in virtuoso root
          (setq split-root (split-string skill-virtuoso-root "/"))
          (setq skill-virtuoso-root "/")
          (while (cdr split-root)
            (setq skill-virtuoso-root (concat skill-virtuoso-root "/" (pop split-root)))
          );while
        );while
        (setq skill-doc-root (concat skill-virtuoso-root skill-doc-root))
      );let
    );when
  );unless
  ;; Documentation root should be found, fetching modules
  (let (modules module-root functions)
    (when (file-directory-p skill-doc-root)
      (set 'modules (directory-files skill-doc-root nil "^[a-zA-Z_]*$"))
    )
    ;; fetching functions or forms in modules
    (dolist (module modules)
      (setq module-root (concat skill-doc-root "/" module))
      ;; only fetching modules defined in settings
      (cond
        ;; Core functions are considered as forms
        ((equal module "Core_SKILL")
          (dolist (file (directory-files module-root t "^.*\.fnd$"))
            (dolist (function (skill-fetch-functions file))
              (push function skill-forms)
            );dolist function
          );dolist file
        )
        ;; Others modules contains classic functions
        ((member module skill-modules-highlights)
          ;; fetching all .fnd files in module
          (dolist (file (directory-files module-root t "^.*\.fnd$"))
            (dolist (function (skill-fetch-functions file))
              ;; fetching functions defined in .fnd file
              (push function skill-functions)
            );dolist function
          );dolist file
        )
        (t
          ;; checking if module exists but has prefixes list
          (dolist (couple skill-modules-highlights)
            (when (listp couple)
              (when (equal module (car couple))
                (dolist (file (directory-files module-root t "^.*\.fnd$"))
                  (dolist (function (apply 'skill-fetch-functions (cons file (cdr couple))))
                    (push function skill-functions)
                  );dolist
                );dolist
              );when
            );when
          );dolist
        );t
      );cond
    );dolist
    ;; fetching functions in doc files
    (dolist (file skill-doc-files)
      (dolist (function (skill-fetch-functions file))
        (push function skill-functions)
      );dolist function
    );dolist file
    ;; removing duplicates
    (delete-dups skill-functions)
    (delete-dups skill-forms)
  );let
);defun

;; =================================================================================
;; Fetching functions defined in user's code if root is correct
;; =================================================================================

(defun skill-fetch-in-code (file)
  ;; Fetch all the functions, classes and methods defined in file
  ;; file is the path to a .il or .ils file
  (let (type regexp point mark)
    ;; copying file content in a new buffer
    (get-buffer-create "*skill-file-content*")
    (switch-to-buffer "*skill-file-content*")
    (insert-file-contents file)
    ;; fetching associated elements for each type
    (dolist (couple
        '(
          (skill-functions "^\(? *\\(defun\\|define\\|procedure\\)\(? *")
          (skill-classes "^\(? *defclass\(? *")
          (skill-methods "^\(? *defmethod\(? *")
        )
      )
      (setq type (pop couple))
      (setq regexp (pop couple))
      ;; fetching functions/classes/methods and adding it to the corresponding lists
      (while (search-forward-regexp regexp nil t)
        (set 'point (point))
        (when (search-forward-regexp "[a-zA-Z_0-9]*" nil t)
          (set 'mark (point))
          (eval `(push ,(buffer-substring point mark) ,type))
        )
      )
    );foreach
    ;; killing the created buffer
    (kill-buffer "*skill-file-content*")
  );let
);defun

(defun skill-fetch-code nil
  ;; Fetching functions in user's code
  (when (and (boundp 'skill-code-root) skill-code-root)
    ;; checking if the root describes a path
    (when (and (stringp skill-code-root) (file-directory-p skill-code-root)) (setq skill-code-root (list skill-code-root)))
    (let
      ((directories skill-code-root) directory files)
      ;; fetching all skill files in all directories from the root
      (while directories (set 'directory (pop directories))
        ;; checking if the current directory is valid
        (when (file-directory-p directory)
          ;; adding directories and files to the list of directories
          (when skill-recursive-search
            (set 'directories (append (directory-files directory t "^[a-zA-Z_]*$") directories))
          )
          ;; adding SKILL and SKILL++ files to the list of files to parse
          (set 'files (append (directory-files directory t "^.*\\.ils?$") files))
        )
      )
      ;; fetching functions/classes/methods in each file
      (dolist (file files)
        (when (file-readable-p file)
          (skill-fetch-in-code file)
        );when
      );foreach
      ;; purging the lists
      (mapcar 'delete-dups (list skill-functions skill-forms skill-classes skill-methods))
    );let
  );when
);progn

(provide 'skill-fetch)

