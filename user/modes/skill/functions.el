;; =================================================================================
;; Functions used by SKILL mode
;; 
;; A. BUCHET
;; =================================================================================

;; =================================================================================
;; Creating Shortcuts
;; =================================================================================

(defun skill-delete-backward ()
  "Deletes previous char. If it is a blank space: deletes all neighboring blank spaces in a row"
  (interactive)
  (if (eq (char-before) ?\ )
    (while (eq (char-before) ?\ )
      (delete-backward-char 1)
    )
    (delete-backward-char 1)
  );if
);defun

(defun skill-delete-forward ()
  "Deletes next char. If it is a blank space: deletes all neighboring blank spaces in a row"
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

(defun skill-insert-parentheses ()
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

(defun skill-insert-comment ()
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
            (when (and (eq (char-before) ?\)) (not (skill-is-line-full)))
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
  "Change SKILL written in C style to lisp style in region"
  (interactive)
  (let (
      (minBound (region-beginning))
      (maxBound (region-end))
    )
    (save-excursion
      (goto-char minBound)
      (while (search-forward-regexp "\\<[a-zA-Z\\_]+\(" maxBound t)
          (push-mark)
          (search-backward-regexp "\\<[a-zA-Z\\_]+\(" minBound t)
            (insert ?\()
              (insert (buffer-substring (point) (- (mark) 1)))
              (insert ?\s)
              (delete-region (point) (mark))
            );while
          );save-excursion
        );let
      );defun

(defun skill-is-line-full ()
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
 
(defun skill-indent-line ()
  "Indent current line of SKILL code"
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
      (when (and (< d-par 0) (not (skill-is-line-full))) (set 'd-par 0))
      (set 'indent-level  (+ (current-indentation) (* tab-length d-par)))
      ;; calculating the indentation level of the current line if it contains only parentheses
      (next-line)
      (unless (skill-is-line-full)
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

(defun skill-newline-and-indent ()
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
            (skill-indent-line)
            (next-line);because the last line is not indented
            (skill-indent-line)
          );save-excursion
        );while
      );save-excursion
      ;; indenting the first line
      (save-excursion
        (newline)
        (previous-line)
        (skill-indent-line)
        (next-line)
        (skill-indent-line)
      );save-excursion
    );progn
    ;; when ther is no parentheses we just add newline and indent
    (newline)
    (save-excursion
      (previous-line)
      (skill-indent-line)
    );save-excursion
    (skill-indent-line)
  );if
);defun

(defun skill-insert-title ()
  (interactive)
  (let ((title (read-string "Title ")))
    (unless (bobp) (insert "\n"))
    (insert ";; =================================================================================\n")
    (insert ";; " title "\n")
    (insert ";; =================================================================================\n")
;    (next-line)
    );let
  );defun 

(defun skill-comment-region ()
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

(defun skill-add-fun ()
  ;;add a prompted function name to the keywords list
  (interactive)
  (push (read-string "Function ") skill-functions)
  (skill-reload)
);defun 

;; =================================================================================
;; Loading the current file with virtuoso
;; =================================================================================

(defun skill-load-file nil
  "Load the current SKILL file in virtuoso"
  (interactive)
  (let
    (
      (fileName (buffer-file-name))
    )
    ;; saving current buffer
    (save-buffer)
    ;; sending commands to virtuoso using stdout
    (append-to-file 
      (concat
        "(progn "
          ;; printing that file is loaded
          "(printf \"*** Emacs *** loading " fileName "\\n\") "
          ;; loading the file
          "(load \"" fileName "\")"
          ")\n"
      )
      nil "/dev/stdout"
    )
  )
)

;; =================================================================================
;; Opening the log if necessary and clean it
;; =================================================================================

(defun skill-clean-log nil
  "Opens log window if it is not displayed, then center the end of log in window"
  (interactive)
  (let
    (
      (windows (window-list))
      window current-window
    )
    (setq current-window (car (window-list)))
    ;; browsing displayed windows to find skill.log if it exists
    (while (and
        (setq window (pop windows))
        (not (equal (or (buffer-file-name (window-buffer window)) "") skill-log-path))
      )
      nil
    )
    (if window
      ;; log is displayed, select window
      (select-window window)
      ;; splitting window in 2 vertical windows
      (delete-other-windows)
      (split-window-horizontally)
      (other-window 1)
      ;; creating Lint buffer
      (view-file "/home/buchetau/SKILL/skill.log")
      ;; displaying skill-log-buffer with auto-revert-mode
      ;(auto-revert-mode t)
    )
    ;; goto end of log and center it
    (goto-char (point-max))
    (recenter)
    (select-window current-window)
  )
)

;; =================================================================================
;; Using Lint on current file
;; =================================================================================

(defun skill-lint-file nil
  "Use Lint on the current SKILL file"
  (interactive)
  (let
    (
      (fileName (buffer-file-name))
    )
    ;; saving current buffer
    (save-buffer)
    ;; sending commands to virtuoso using stdout
    (append-to-file 
      (concat 
        "(progn "
          ;; printing that Lint is used
          "(printf \"*** Emacs *** Using Lint on " fileName "\\n\") "
          ;; using Lint
          "(sklint ?file \"" fileName "\"))\n"
      )
      nil "/dev/stdout"
    )
  )
)

;; =================================================================================
;; Evaluating selected region
;; =================================================================================

(defun skill-eval-region nil
  "Evaluates the selected region in virtuoso and returns the result in the skill log"
  (interactive)
  ;; sending commands to virtuoso
  ;; evaluating the selected lines
  (append-to-file
    (concat
      "(progn "
        "(printf \"*** Emacs *** Evaluating region\\n\") "
        "(mapcar (lambda (x) (println x) (println (eval x)) (printf \"\\n\")) '( " (buffer-substring-no-properties (mark) (point)) ")) "
        ")\n"
    )
    nil "/dev/stdout"
  )
)

;; =================================================================================
;; Evaluating buffer
;; =================================================================================

(defun skill-eval-buffer nil
  "Evaluates the current buffer in virtuoso and returns the result in the skill log"
  (interactive)
  ;; sending commands to virtuoso
  ;; evaluating the selected lines
  (append-to-file
    (concat
      "(progn "
        "(printf \"*** Emacs *** Evaluating Buffer\\n\") "
        "(mapcar (lambda (x) (println x) (println (eval x)) (printf \"\\n\")) '( " (buffer-substring-no-properties (point-min) (point-max)) ")) "
        ")\n"
    )
    nil "/dev/stdout"
  )
)

;; =================================================================================
;; Using Trace on selected symbol
;; =================================================================================

(defun skill-trace (start end mode)
  "Prompt user for a mode and trace a symbol using the selected mode (by default uses the symbol in region)"
  (interactive "r\nkTrace mode: ")
  (let
    (
      (symbol (buffer-substring-no-properties start end))
    )
    ;; prompt user to know which symbol trace
    (setq symbol
      (read-string "Symbol to Trace: "
        (when (< (abs (- end start)) 30) symbol)
      )
    )
    ;; checking if mode is variable, function or prop, doing nothing otherwise
    (when (member mode '("v" "f" "p"))
      (append-to-file
        (concat
          "(progn "
            "(printf \"*** Emacs *** Tracing " symbol "\\n\")"
            "(trace" mode " " symbol")"
            ")\n"
        )
        nil "/dev/stdout"
      )
    )
  )
)

(defun skill-untrace (start end mode)
  "Prompt user for a mode and untrace a symbol using the selected mode (by default uses the symbol in region)"
  (interactive "r\nkTrace mode: ")
  (let
    (
      (symbol (buffer-substring-no-properties start end))
    )
    ;; prompt user to know which symbol trace
    (setq symbol
      (read-string "Symbol to Trace: "
        (when (< (abs (- end start)) 30) symbol)
      )
    )
    ;; checking if mode is variable, function or prop, doing nothing otherwise
    (when (member mode '("v" "f" "p"))
      (when (equal mode "f") (setq mode ""))
      (append-to-file
        (concat
          "(progn "
            "(printf \"*** Emacs *** Tracing " symbol "\\n\")"
            "(untrace" mode " " symbol")"
            ")\n"
        )
        nil "/dev/stdout"
      )
    )
  )
)

(provide 'skill-functions)
