;----------------------------------------------------------------------------------------
;Creating Shortcuts:
;----------------------------------------------------------------------------------------

(defvar lisp-map
  (let (
	(lisp-mode-map (make-keymap))
       )
    (define-key lisp-mode-map "(" 'lisp-insert-parentheses)
    (define-key lisp-mode-map "\"" 'insert-quote-marks)
    (define-key lisp-mode-map ")" 'check-closed-parentheses)
    (define-key lisp-mode-map (kbd "<return>") 'lisp-newline-and-indent)
    (define-key lisp-mode-map (kbd "M-;") 'lisp-comment-region)
    (define-key lisp-mode-map (kbd "C-;") 'lisp-insert-title)
    (define-key lisp-mode-map ";" 'lisp-insert-comment)
    (define-key lisp-mode-map (kbd "<backspace>") 'lisp-delete-backward)
    (define-key lisp-mode-map (kbd "C-d") 'lisp-delete-forward)
    lisp-mode-map
  )
  "Keymap for LISP major mode"
);defvar

(defun lisp-delete-backward ()
  (interactive)
  (if (eq (char-before) ?\ )
    (while (eq (char-before) ?\ )
      (delete-backward-char 1)
    )
    (delete-backward-char 1)
  );if
);defun

(defun lisp-delete-forward ()
  (interactive)
  (if (eq (char-after) ?\ )
    (while (eq (char-after) ?\ )
      (delete-char 1)
    )
    (delete-char 1)
  );if
);defun

(defun insert-quote-marks ()
  "Insert quote mark with a different behavior depending on the case"
  (interactive)
  (if (= (char-before) ?\")
      ;if char before the point is a " with text before, we're moving the " behind the next word
      (if (string-match "[a-zA-Z_]" (buffer-substring (- (point) 2) (- (point) 1)))
          (progn (delete-region (- (point) 1) (point))
                 (forward-char)
                 (search-forward-regexp "[a-zA-Z_]*")
                 (insert "\""))
        ;if char before the point is a " without text before, we're closing the " and placing the point between ""
        (if (member (char-after) '(? ?\ ?\n ?\)))
            (progn
              (insert "\"")
              (backward-char)
              )
          (search-forward-regexp "[a-zA-Z_]*")
          (insert "\"")
          )
        );if
    ;in the other cases we are just placing "
    (insert "\"")
    );if
  );defun

(defun lisp-insert-parentheses ()
"Insert pair of parentheses or just open parenthese depending on the point"
  (interactive)
  ;if char after is space, newline or ) we're writing () and placing the point between
  (if (member (char-after) '(? ?\ ?\n ?\) nil))
      (progn
        (insert "\(\)")
        (backward-char)
        )
    (insert "\(")
    );if
  );defun

(defun lisp-insert-comment ()
"Insert two ; and a space after"
  (interactive)
  (let (beg end)
    (save-excursion
      (beginning-of-line)
      (setq beg (point))
      (end-of-line)
      (setq end (point))
      );save-excursion
    (if (string-match "^[\s\n]*$" (buffer-substring beg end))
        (insert ";; ")
      (insert ";")
      );if
    );let
  );defun
  
(defun count-parentheses-line ()
"Count parentheses on the line and return a couple: (open closed)"
  (interactive)
  (save-excursion
    (end-of-line)
    (let ((i 0) (n (point)) (co 0) (cc 0))
      (beginning-of-line)
      (set 'i (point))
      (while (< i n)
        (when (eq (char-after i) ?\() (set 'co (+ co 1)))
        (when (eq (char-after i) ?\)) (set 'cc (+ cc 1)))
        (set 'i (+ i 1))
        )
      (cons co (cons cc ()))
      );; let
    );; save-excursion
  );; defun

(defun check-closed-parentheses ()
  "If the pointer is between ) and the end of the line, it adds a comment containing the function used at the opening of the correspondant parenthese, else it only insert a )" 
  (interactive)
  (let ((parentheses (count-parentheses-line)) (i (- (point) 1)) (c 1) string)
    (cond
      ;; if the point is after the only one closing parentheses on the line
      ((and (= (char-before) ?\)) (= (pop parentheses) 0) (= (pop parentheses) 1))
        (progn
          ;; fetching the associated opening parentheses
          (while (and (> i 0) (> c 0))
            (set 'i (- i 1))
            (when (eq (char-after i) ?\() (set 'c (- c 1)))
              (when (eq (char-after i) ?\)) (set 'c (+ c 1)))
          )
          ;; fetching the word after the opening parentheses
          (save-excursion
            (goto-char i)
            (push-mark)
            (search-forward-regexp "\([a-zA-Z\_]*")
            (set 'string (buffer-substring (+ 1 (mark)) (point)))
          )
          ;; inserting the fetched word as a commentary
          (insert ?\; string)
          ;; fetching the same thing recursively for precedent line
          (save-excursion
            (previous-line)
            (end-of-line)
            (when (and (eq (char-before) ?\)) (not (lisp-is-line-full)))
              (check-closed-parentheses)
            );when
          );save-excursion
        );progn
      );point after the only one closing parentheses of the line
      ;; if point is after an opening parentheses we places the closing parentheses after the next word
      ((and (= (char-before) ?\() (string-match "[a-zA-Z_]" (buffer-substring (point) (+ (point) 1))))
        (while (string-match "[a-zA-Z_]" (buffer-substring (point) (+ (point) 1)))
          (forward-char)
        );while
        (insert ")")
        (backward-char) 
      );point after an opening parentheses
      (t
        (insert ")")
      )
    );cond
  );let
);defun

(defun c-to-lisp ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (search-forward-regexp "\\<[a-zA-Z\_]+\(")
      (push-mark)
      (search-backward-regexp "\\<[a-zA-Z\_]+\(")
      (insert ?\()
      (insert (buffer-substring (point) (- (mark) 1)))
      (insert ?\s)
      (delete-region (point) (mark))
      );while
    );save-excursion
  );defun
  
(defun lisp-is-line-full ()
  "returns t if line contains only parentheses"
  (save-excursion
    (let ((test nil))
      (beginning-of-line)
      (while (not (or test (member (char-after) '(?\n ?\;))))
        (unless (member (char-after) '(?\ ?\))) (setq test t))
        (forward-char)
      );while
      test
    );let
  );save-excursion
);defun

(defun delete-extra-spaces ()
  "deleting all extra spaces"  
  (interactive)
  (save-excursion
    (beginning-of-line)
    (while (not (eolp))
      (while 
          (and 
            (eq (char-after) ?\s)
            (or
              (member (char-after (+ (point) 1)) '(?\) ?\s))
              (= (char-before) ?\()
            );or
          );and
        (delete-region (point) (+ (point) 1))
      );while
      (forward-char)
    );while
  );save-excursion
);defun
 
(defun lisp-indent-line ()
  "Indent current line of LISP code"
  (interactive)
  (let 
    (
      (current-indent-level (current-indentation)) 
      (current-point (point))
      end-of-line-point
      beginning-of-line-point 
      indent-level 
      parentheses 
      d-par 
      (tab-length 2)
    )
    (delete-extra-spaces)
    ;; fetching end and beginning of line
    (end-of-line)
    (set 'end-of-line-point (point))
    (beginning-of-line)
    (set 'beginning-of-line-point (point))
    (indent-line-to 0)
    (unless (bobp)
      ;; calculating the indentation level of the previous line
      (previous-line)
      (set 'parentheses (count-parentheses-line))
      (set 'd-par (- (pop parentheses) (pop parentheses)))
      (when (and (< d-par 0) (not (lisp-is-line-full))) (set 'd-par 0))
      (set 'indent-level  (+ (current-indentation) (* tab-length d-par)))
      ;; calculating the indentation level of the current line if it contains only parentheses
      (next-line)
      (unless (lisp-is-line-full)
        (set 'parentheses (count-parentheses-line))
        (set 'd-par (- (pop parentheses) (pop parentheses)))
        ;(unless (> d-par 0) (set 'indent-level  (+ indent-level (* tab-length d-par))))
        ;; reducing the max decrease number to one
        (when (< d-par 0) (set 'indent-level  (+ indent-level (* tab-length d-par))))
      );unless
      (indent-line-to indent-level)
      ;; saving the excursiom
      (if (> indent-level current-indent-level)
        (goto-char (+ current-point (- indent-level current-indent-level)))
        (if (> (- current-point beginning-of-line-point) (- current-indent-level indent-level))
          (goto-char (- current-point (- current-indent-level indent-level)))
          (beginning-of-line)
        );if
      );if
    );unless
  );let  
);defun

(defun lisp-newline-and-indent ()
  (interactive)
  (if (member (char-after) '(?\) ?\}))
    ;; when there are parentheses after the point we indent them correctly
    (progn
      (save-excursion
        (while (and (eq (char-after) ?\)) (eq (char-after (+ (point) 1)) ?\)))
          (forward-char)
          (newline)
          (save-excursion
            (previous-line)
            (lisp-indent-line)
            (next-line);because the last line is not indented
            (lisp-indent-line)
          );save-excursion
        );while
      );save-excursion
      ;; indenting the first line
      (save-excursion
        (newline)
        (previous-line)
        (lisp-indent-line)
        (next-line)
        (lisp-indent-line)
      );save-excursion
    );progn
    ;; when ther is no parentheses we just add newline and indent
    (newline)
    (save-excursion
      (previous-line)
      (lisp-indent-line)
    );save-excursion
    (lisp-indent-line)
  );if
);defun

(defun lisp-insert-title ()
  (interactive)
  (let ((title (read-string "Title ")))
    (unless (bobp) (insert "\n"))
    (insert ";; =================================================================================\n")
    (insert ";; " title "\n")
    (insert ";; =================================================================================\n")
;    (next-line)
    );let
  );defun 

(defun lisp-comment-region ()
  (interactive)
  ;; setting the role of the function (commenting or uncommenting depending on the first line of the region)
  (let ((role nil) (e (region-end)))
    (when (> (point) (mark)) (exchange-point-and-mark))
    (beginning-of-line)
    ;; if the first line is commented, role is set to uncomment (t)
    (when (= (char-after) ?\;) (setq role t))
    (while (<= (point) e)
      (beginning-of-line)
      (if (= (char-after) ?\;)
        (when role (delete-region (point) (+ 1 (point))))
        (when (not role)
          (insert ";")
          (backward-char)
        );when
      );if
      (next-line)
    );while
  );let
);defun

(defun lisp-add-fun ()
  ;;add a prompted function name to the keywords list (if name is empty fetches names in lisp files)
  (interactive)
  (let ((function (read-string "Function ")))
    (find-file "~/.emacs.d/user/modes/emacs_lisp/1_variables.el")
    ;; placing the point at the end of regexp-opt
    (search-forward-regexp "lisp-functions\n  '(")
    (search-forward-regexp "\)\n")
    (search-backward-regexp "\)\n")
    ;; inserting the name of the function
    (insert "\"" function "\" ")
    (save-buffer)
    (kill-buffer "1_variables.el")
    (push function lisp-functions)
    ;; actualising lisp-font-lock
    (setq lisp-font-lock
      (list
        ;; Order is important ! First it matchs functions then methods then classes etc
        ;; macros
        (cons (concat "\\<" (regexp-opt lisp-macros t) "\\>") ''user-form-face); two ' because emacs-basic-faces are associated to a symbol returning their name [(eval font-lock-variable-name-face) returns font-lock-variable-name-face]
        ;; functions
        (cons (concat "\\<" (regexp-opt lisp-functions t) "\\>") ''user-function-face)
        ;; forms
        (cons (concat "\\<" (regexp-opt lisp-forms t) "\\>") ''user-class-face)
        ;; symbols
        (cons "\'[a-zA-Z_]*\\>" ''user-symbol-face)
        ;; variables
        (cons "[@\?][a-zA-Z_]*\\>" ''user-variable-face)
        ;; keywords (and prefixes)
        (cons (concat "\\<" (regexp-opt '("p" "w" "g" "d" "l" "x" "f" "r" "s" "o" "t" "nil" "()") t) "\\>") ''user-keyword-face)
      );list
    );setq
    (lisp-mode)
  );let
);defun 

(defun lisp-add-macro ()
  ;;add a prompted function name to the keywords list (if name is empty fetches names in lisp files)
  (interactive)
  (let ((function (read-string "Function ")))
    (find-file "~/.emacs.d/user/modes/emacs_lisp/1_variables.el")
    ;; placing the point at the end of regexp-opt
    (search-forward-regexp "lisp-macros\n  '(")
    (search-forward-regexp "\)\n")
    (search-backward-regexp "\)\n")
    ;; inserting the name of the function
    (insert "\"" function "\" ")
    (save-buffer)
    (kill-buffer "1_variables.el")
    (push function lisp-macros)
    ;; actualising lisp-font-lock
    (setq lisp-font-lock
      (list
        ;; Order is important ! First it matchs functions then methods then classes etc
        ;; macros
        (cons (concat "\\<" (regexp-opt lisp-macros t) "\\>") ''user-form-face); two ' because emacs-basic-faces are associated to a symbol returning their name [(eval font-lock-variable-name-face) returns font-lock-variable-name-face]
        ;; functions
        (cons (concat "\\<" (regexp-opt lisp-functions t) "\\>") ''user-functions-face)
        ;; forms
        (cons (concat "\\<" (regexp-opt lisp-forms t) "\\>") ''user-class-face)
        ;; symbols
        (cons "\'[a-zA-Z_]*\\>" ''user-symbol-face)
        ;; variables
        (cons "[@\?][a-zA-Z_]*\\>" ''user-variable-face)
        ;; keywords (and prefixes)
        (cons (concat "\\<" (regexp-opt '("p" "w" "g" "d" "l" "x" "f" "r" "s" "o" "t" "nil" "()") t) "\\>") ''user-keyword-face)
      );list
    );setq
    (lisp-mode)
  );let
);defun 
