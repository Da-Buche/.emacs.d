;; ===============================================================================================================
;; Load recursively all files from .emacs.d directory
;;
;; A. Buchet - September 2024
;; ===============================================================================================================

;; Find recursively .el files in autoloaded directory
;; (`ignore-errors' call to support older emacs versions as well)
(let ( (files (or (ignore-errors (directory-files-recursively (expand-file-name "~/.emacs.d/autoloaded") ".el$" nil t t))
                                 (directory-files-recursively (expand-file-name "~/.emacs.d/autoloaded") ".el$" nil)
                  ))
       )

  (dolist (file files)

    ;; Prevent reloading current file to avoid infinite loop
    (let ( (current-file (or load-file-name "~/.emacs.d/load.el"))
           )
      (unless (equal (file-truename file)
                     (file-truename current-file)
                     )
        (with-demoted-errors (format "Loading %s" file) (load file))
        ));unless ;let
    ));dolist ;let

;; External project 'formatter' contains command to reshape SKILL to Lisp or C-style
;(load (expand-file-name "~/projects/formatter/emacs-lisp/format.el"))

