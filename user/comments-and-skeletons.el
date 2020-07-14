;; =============================================================================================================
;; Smart comments insertion and personal skeletons
;; 
;; Author: Aurelien Buchet
;; =============================================================================================================

(require 'sh-script)

;; Remove space in shell and python comments by default
(dolist (mode-hook '(python-mode-hook sh-mode-hook))
  (add-hook mode-hook (lambda nil (when (equal comment-start "# ") (setq comment-start "#")))))
  
(defun global-insert-comment nil 
  "In an empty line: insert two comment-start and a space after. On a normal line: insert comment-start." 
  (interactive)
  (let (beg end)
    ;; Reset comment-start to ";" when badly defined
    (when (or (not comment-start) (equal comment-start " ")) (setq comment-start ";"))
    ;; Check if current string is empty
    (save-excursion (beginning-of-line) (setq beg (point)) (end-of-line) (setq end (point)))
    (if (not (string-match "^[\s\n]*$" (buffer-substring beg end))) (insert comment-start)
      ;; String is empty, inserts two comments and a space
      (insert comment-start comment-start " ")
      (when comment-end (save-excursion (insert comment-end))))
  );let
);defun

(global-set-key (kbd ";") 'global-insert-comment)

(defvar comment-separator-length 111
  "Length of header separator. It corresponds to the number of '=' or '-' after the ';; ' comment prefix")

(defun global-insert-semicolon nil "Inserts a semicolon" (interactive) (insert ";"))

(defun global-insert-file-title nil
  "Insert file title banner"
  (interactive)
  (let ((separator (concat comment-start comment-start " "))
        (length (if comment-separator-length comment-separator-length (- (window-width) 3))))
    ;; Set separator line
    (dotimes (number length) (setq separator (concat separator "=")))
    ;; Skip a line if point is not at beginning of buffer
    (unless (bobp)                                                    (funcall indent-line-function) (insert "\n"))
    (insert separator)                                                (funcall indent-line-function) (insert "\n")
    (insert comment-start comment-start " ")                          (funcall indent-line-function)
    (save-excursion (insert "\n")
      (insert comment-start comment-start " ")                        (funcall indent-line-function) (insert "\n")
      (insert comment-start comment-start " A. Buchet ")              (funcall indent-line-function) (insert "\n")
      (insert separator)                                              (funcall indent-line-function) (insert "\n"))))

(defun global-insert-title (level char)
  "Insert a commenttitle using CHAR, size of the title depends on LEVEL"
  (unless (< 0 level) (setq level 1))
  (let* ((prefix (concat comment-start comment-start " "))
         (length (if comment-separator-length comment-separator-length (- (window-width) 3)))
         (separator prefix))
    ;; Define separator line
    (setq length (/ length level))
    (dotimes (number length) (setq separator (concat separator char)))
    ;; Check if current line is a comment
    (if (nth 4 (syntax-ppss))
        ;; In a comment, insert separators around current line
        (save-excursion
          (beginning-of-line) (funcall indent-line-function) (insert separator "\n")
          (funcall indent-line-function)
          (end-of-line) (insert "\n" separator) (funcall indent-line-function))
      ;; Insert two separators with a line containing point in the middle
      (insert separator) (funcall indent-line-function) (insert "\n")
      (insert prefix) (funcall indent-line-function) ; point stays here
      (save-excursion (insert "\n") (insert separator) (funcall indent-line-function))
      );if
    );let
  );defun

(defun global-insert-function nil (interactive)
  ;; Insert a function header
  (let ((separator (concat comment-start comment-start " "))
      (length (if comment-separator-length comment-separator-length
          (- (window-width) 3))))
    ;; Defining the separator line (inserting '=' to fill the line)
    (dotimes (number length) (setq separator (concat separator "-")))
    ;; Newline unless point is at beginning of buffer
    (unless (bobp) (insert "\n"))
    (insert separator "\n"
      comment-start comment-start " @fun ")
    ;; Asking user for function name
    (insert (read-string "Function ") "\n"
      comment-start comment-start "\n"
      comment-start comment-start " @doc ")
    ;; Asking user for function doc
    (insert (read-string "Doc "))
    (save-excursion (insert "\n" separator))))

(defun global-insert-macro nil (interactive)
  ;; Insert a macro header
  (let ((separator (concat comment-start comment-start " "))
      (length (if comment-separator-length comment-separator-length
          (- (window-width) 3))))
    ;; Defining the separator line (inserting '=' to fill the line)
    (dotimes (number length) (setq separator (concat separator "-")))
    ;; Newline unless point is at beginning of buffer
    (unless (bobp) (insert "\n"))
    (insert separator "\n"
      comment-start comment-start " @macro ")
    ;; Asking user for function name
    (insert (read-string "Macro ") "\n"
      comment-start comment-start "\n"
      comment-start comment-start " @doc ")
    ;; Asking user for function doc
    (insert (read-string "Doc "))
    (save-excursion (insert "\n" separator))))

(defun global-insert-variable nil (interactive)
  ;; Insert a variable header
  (let ((separator (concat comment-start comment-start " "))
      (length (if comment-separator-length comment-separator-length
          (- (window-width) 3))))
    ;; Defining the separator line (inserting '=' to fill the line)
    (dotimes (number length) (setq separator (concat separator "-")))
    ;; Newline unless point is at beginning of buffer
    (unless (bobp) (insert "\n"))
    (insert separator "\n"
      comment-start comment-start " @var ")
    ;; Asking user for function name
    (insert (read-string "Variable ") "\n"
      comment-start comment-start "\n"
      comment-start comment-start " @doc ")
    ;; Asking user for function doc
    (insert (read-string "Doc "))
    (save-excursion (insert "\n" separator))))

(defun global-insert-arg nil (interactive)
  ;; Insert a param in a header
  (insert "\n" comment-start comment-start "\n"
    comment-start comment-start " @arg ")
  ;; Asking user for param name
  (insert (read-string "Argument ") "\n"
    comment-start comment-start " @type ")
  ;; Asking user for param type
  (insert (read-string "Type ") "\n"
    comment-start comment-start " @doc ")
  ;; Asking user for param doc
  (insert (read-string "Doc ")))

(defun global-insert-out nil (interactive)
  ;; Insert a param in a header
  (insert "\n" comment-start comment-start "\n"
    comment-start comment-start " @out ")
  ;; Asking user for param name
  (insert (read-string "Output ") "\n"
    comment-start comment-start " @type ")
  ;; Asking user for param type
  (insert (read-string "Type ") "\n"
    comment-start comment-start " @doc ")
  ;; Asking user for param doc
  (insert (read-string "Doc ")))

;; Defining insert map (bound to C-;, displays 'Insert:' when enabled)
(define-prefix-command 'insert-map nil "Insert")
(global-set-key (kbd "C-;") insert-map)

(define-key insert-map "-" (lambda (n) (interactive "^p") (unless n (setq n 1)) (global-insert-title n "-")))
(define-key insert-map "=" (lambda (n) (interactive "^p") (unless n (setq n 1)) (global-insert-title n "=")))

(define-key insert-map ";" 'global-insert-semicolon)
(define-key insert-map "b" 'global-insert-file-title)
(define-key insert-map "t" 'global-insert-title)
(define-key insert-map "s" 'global-insert-light-title)

(define-key insert-map "a" 'global-insert-arg)
(define-key insert-map "o" 'global-insert-out)
(define-key insert-map "f" 'global-insert-function)
(define-key insert-map "v" 'global-insert-variable)
(define-key insert-map "c" 'global-insert-class)
(define-key insert-map "m" 'global-insert-macro)


