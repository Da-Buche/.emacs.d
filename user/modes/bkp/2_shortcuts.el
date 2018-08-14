;----------------------------------------------------------------------------------------
;Creating Shortcuts:
;----------------------------------------------------------------------------------------

(defvar bkp-map
  (let (
	(bkp-mode-map (make-keymap))
       )
    (define-key bkp-mode-map "(" 'bkp-insert-parentheses)
    (define-key bkp-mode-map "\"" 'insert-quote-marks)
    (define-key bkp-mode-map ")" 'check-closed-parentheses)
    (define-key bkp-mode-map (kbd "<return>") 'bkp-newline-and-indent)
    (define-key bkp-mode-map (kbd "M-;") 'bkp-comment-region)
    (define-key bkp-mode-map (kbd "C-;") 'bkp-insert-title)
    (define-key bkp-mode-map ";" 'bkp-insert-comment)
    (define-key bkp-mode-map (kbd "<backspace>") 'bkp-delete-backward)
    (define-key bkp-mode-map (kbd "C-d") 'bkp-delete-forward)
    bkp-mode-map
  )
  "Keymap for BKP major mode"
);defvar

(defun bkp-delete-backward ()
  (interactive)
  (if (eq (char-before) ?\ )
    (while (eq (char-before) ?\ )
      (delete-backward-char 1)
    )
    (delete-backward-char 1)
  );if
);defun

(defun bkp-delete-forward ()
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

(defun bkp-insert-parentheses ()
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

(defun bkp-insert-comment ()
"Insert * and a space after"
  (interactive)
  (insert "*")
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
            (when (and (eq (char-before) ?\)) (not (bkp-is-line-full)))
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
  
(defun bkp-is-line-full ()
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
 
(defun bkp-indent-line ()
  "Indent current line of BKP code"
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
      (when (and (< d-par 0) (not (bkp-is-line-full))) (set 'd-par 0))
      (set 'indent-level  (+ (current-indentation) (* tab-length d-par)))
      ;; calculating the indentation level of the current line if it contains only parentheses
      (next-line)
      (unless (bkp-is-line-full)
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

(defun bkp-newline-and-indent ()
  (interactive)
  (newline)
);defun

(defun bkp-insert-title ()
  (interactive)
  (let ((title (read-string "Title ")))
    (unless (bobp) (insert "\n"))
    (insert ";; =================================================================================\n")
    (insert ";; " title "\n")
    (insert ";; =================================================================================\n")
;    (next-line)
    );let
  );defun 

(defun bkp-comment-region ()
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

(defun bkp-add-fun ()
  ;;add a prompted function name to the keywords list (if name is empty fetches names in bkp files)
  (interactive)
  (let ((function (read-string "Function ")))
    (find-file "~/.emacs.d/user/modes/emacs_bkp/1_variables.el")
    ;; placing the point at the end of regexp-opt
    (search-forward-regexp "bkp-functions\n  '(")
    (search-forward-regexp "\)\n")
    (search-backward-regexp "\)\n")
    ;; inserting the name of the function
    (insert "\"" function "\" ")
    (save-buffer)
    (kill-buffer "1_variables.el")
    (push function bkp-functions)
    ;; actualising bkp-font-lock
    (setq bkp-font-lock
      (list
        ;; Order is important ! First it matchs functions then methods then classes etc
        ;; macros
        (cons (concat "\\<" (regexp-opt bkp-macros t) "\\>") ''user-form-face); two ' because emacs-basic-faces are associated to a symbol returning their name [(eval font-lock-variable-name-face) returns font-lock-variable-name-face]
        ;; functions
        (cons (concat "\\<" (regexp-opt bkp-functions t) "\\>") ''user-function-face)
        ;; forms
        (cons (concat "\\<" (regexp-opt bkp-forms t) "\\>") ''user-class-face)
        ;; symbols
        (cons "\'[a-zA-Z_]*\\>" ''user-symbol-face)
        ;; variables
        (cons "[@\?][a-zA-Z_]*\\>" ''user-variable-face)
        ;; keywords (and prefixes)
        (cons (concat "\\<" (regexp-opt '("p" "w" "g" "d" "l" "x" "f" "r" "s" "o" "t" "nil" "()") t) "\\>") ''user-keyword-face)
      );list
    );setq
    (bkp-mode)
  );let
);defun 

(defun bkp-add-macro ()
  ;;add a prompted function name to the keywords list (if name is empty fetches names in bkp files)
  (interactive)
  (let ((function (read-string "Function ")))
    (find-file "~/.emacs.d/user/modes/emacs_bkp/1_variables.el")
    ;; placing the point at the end of regexp-opt
    (search-forward-regexp "bkp-macros\n  '(")
    (search-forward-regexp "\)\n")
    (search-backward-regexp "\)\n")
    ;; inserting the name of the function
    (insert "\"" function "\" ")
    (save-buffer)
    (kill-buffer "1_variables.el")
    (push function bkp-macros)
    ;; actualising bkp-font-lock
    (setq bkp-font-lock
      (list
        ;; Order is important ! First it matchs functions then methods then classes etc
        ;; macros
        (cons (concat "\\<" (regexp-opt bkp-macros t) "\\>") ''user-form-face); two ' because emacs-basic-faces are associated to a symbol returning their name [(eval font-lock-variable-name-face) returns font-lock-variable-name-face]
        ;; functions
        (cons (concat "\\<" (regexp-opt bkp-functions t) "\\>") ''user-functions-face)
        ;; forms
        (cons (concat "\\<" (regexp-opt bkp-forms t) "\\>") ''user-class-face)
        ;; symbols
        (cons "\'[a-zA-Z_]*\\>" ''user-symbol-face)
        ;; variables
        (cons "[@\?][a-zA-Z_]*\\>" ''user-variable-face)
        ;; keywords (and prefixes)
        (cons (concat "\\<" (regexp-opt '("p" "w" "g" "d" "l" "x" "f" "r" "s" "o" "t" "nil" "()") t) "\\>") ''user-keyword-face)
      );list
    );setq
    (bkp-mode)
  );let
);defun 


;; =================================================================================
;; Functions for Analog Design
;; =================================================================================

(defun bkp-generate-steps ()
  ;; takes a list of sweeps in argument and generate the list of corresponding points
  (interactive)
  (let* 
    (
      (sweeps (car (read-from-string (read-string "List of (PARAM START STOP ITER): "))))
      sweep 
      l_param
      l_sweeps
    )
    (while (setq sweep (pop sweeps))
      (push (pop sweep) l_param)
      (push (apply 'bkp-generate-sweep sweep) l_sweeps)
    )
    (setq l_sweeps (apply 'bkp-generate-mesh l_sweeps))
    ;; l_sweeps doesn't contain sweeps anymore but a list of points
    (insert ".STEP PARAM ")
    (insert-list l_param)
    (insert " LIST ")
    (while (setq sweep (pop l_sweeps))
      (insert-list sweep)
      (insert " ")
    );while
    (insert sweeps)
  );let
);defun

(defun bkp-generate-mesh (&rest args)
  ;; takes several list of points in arguments and mesh them
  (when args
    (let (
        (sweep (pop args))
        mesh
        point
        old-sweep old-mesh dummy-old-mesh
      )
      (if (not args)
        (mapcar (lambda (x) (cons x nil)) sweep)
        (progn
          (setq old-mesh (apply 'bkp-generate-mesh args))
          (while (setq point (pop sweep))
            (setq dummy-old-mesh old-mesh)
            (while (setq old-sweep (pop dummy-old-mesh))
              (push (cons point old-sweep) mesh)
            );while
          );while
          mesh
        );progn
      );if
    );let
  );when
);defun
;; (bkp-generate-mesh '(1 2) '(3 4))

(defun bkp-generate-sweep (start stop iter)
  (let (
      (delta (/ (- stop start) (float (- iter 1))))
      sweep
    )
    (while (> iter 0)
      (push start sweep)
      (setq start (+ start delta))
      (setq iter (- iter 1))
    );while
    (nreverse sweep)
  );let
);defun
;; (bkp-generate-sweep 0 5 10)

(defun insert-list (l)
  (insert "(")
  (let (e)
    (while (setq e (pop l))
      (insert (prin1-to-string e))
      (insert ", ")
    );while
  );let
  (delete-backward-char 2)
  (insert ")")
);defun
;; (insert-list '(1 2 3))

