;; =================================================================================
;; Simple mode to display properly Cadence CIW in a .log file
;; 
;; Author : Aurelien Buchet
;; =================================================================================

;; =================================================================================
;; Opening .log in skill-log-mode automatically
;; =================================================================================

(add-to-list 'auto-mode-alist '("\\.log\\'" . skill-log-mode))

;; =================================================================================
;; Changing fonts depending on the text
;; =================================================================================

(defvar skill-log-font-lock
  '(
    ;; Emacs
    ("^\\\\o \\*\\*\\* Emacs \\*\\*\\*.*$" . 'user-variable-face)
    ;; Lint SCORE
    ("^\\\\o INFO (IQ).*$"                 . 'user-function-face)
    ;; Lint INFO
    ("^\\\\o INFO.*$"                      . 'user-symbol-face)
    ;; Lint SUGGEST
    ("^\\\\o SUGGEST.*$"                   . 'user-symbol-face)
    ;; Lint HINT
    ("^\\\\o HINT.*$"                      . 'user-string-face)
    ;; Lint WARN
    ("^\\\\o WARN.*$"                      . 'user-comment-face)
    ;; Lint ERR
    ("^\\\\o ERR.*$"                       . 'user-method-face)
    ;; Trace
    ("^\\\\o \|.*$"                        . 'user-basic-face)
    ;; input
    ("^\\\\i.*$"                           . 'user-form-face)
    ;; output
    ("^\\\\o.*$"                           . 'user-symbol-face)
    ;; warning
    ("^\\\\w.*$"                           . 'user-comment-face)
    ;; error
    ("^\\\\e.*$"                           . 'user-method-face)
    ;; separator
    ("^\\\\p.*$"                           . 'user-keyword-face)
    ;; info
    ("^\\\\t.*$"                           . 'user-string-face)
    ;; user
    ("^\\\\a.*$"                           . 'user-class-face)
    ;; return
    ("^\\\\r.*$"                           . 'user-class-face)
  );list
  "Default highlighting expressions for skill-log-mode"
);defvar

;; =================================================================================
;; Hook to hide meta data in log
;; =================================================================================

(add-hook 'skill-log-hooks
  (lambda ()
    (font-lock-add-keywords nil '(("^\\\\." (0 '(face default display "") append))) t)
    (push 'display font-lock-extra-managed-props)
  )
)

;; =================================================================================
;; Funcion to open a skill file
;; =================================================================================

(defun skill-open-file (file &optional lines)
  ;; smart opening of a skill file (switching to a window if it already exists)
  (let
    (
      (windows (window-list)) window
      current-line line first-line
    )
    ;; browsing windows to find one containing a skill file buffer
    (while (and
        (setq window (pop windows))
        (not (string-match "\\.ils?$" (or (buffer-file-name (window-buffer window)) "")))
      )
      nil
    )
    ;; selecting the correct window
    (if (string-match "\\.ils?$" (or (buffer-file-name (window-buffer window)) ""))
      ;; skill window exists, switching to it
      (select-window window)
      ;; no skill window, splitting current window in two vertical ones
      (delete-other-windows)
      (split-window-horizontally)
    )
    ;; opening file
    (find-file file)
    (setq current-line (line-number-at-pos (point)))
    ;; registering first line to allow cycle browsing
    (setq first-line (car lines))
    ;; fetching next line in lines
    (while (and (setq line (pop lines)) (<= line current-line))
      nil
    )
    (unless line (setq line first-line))
    ;; when line was found, going to it
    (when (integerp line)
      (goto-line line buffer)
    )
    ;; removing region
    (unless (equal (mark) (point)) (push-mark))
  )
)

;; =================================================================================
;; Functions to find file and lines in error message and open it
;; =================================================================================

(defun skill-log-click-line nil
  ;; finds file and line (or lines) in the clicked line and opens it
  (interactive)
  (let
    (
      file
      lines line current-line first-line
      eol bol res
      buffer
    )
    ;; fetching file and lines in clicked line
    (save-excursion
      (end-of-line) (setq eol (point))
      (beginning-of-line) (setq bol (point))
      ;; fetching file
      (when
        (progn
          (setq res (search-forward-regexp "[a-zA-Z0-9/\\_\\-\\.]*\\.ils?" nil t))
          (and res (<= res eol))
        )
        (setq file (buffer-substring-no-properties (match-beginning 0) (point)))
      )
      ;; fetching lines
      (when
        ;; lines
        (progn 
          ;; searching "lines (...)" in current line
          (goto-char bol)
          (setq res (search-forward-regexp "lines ([0-9 ]+)" nil t))
          (and res (<= res eol))
        )
        (search-backward-regexp "([0-9 ]+)" nil t)
        (setq lines (sort (read (buffer-substring-no-properties (match-beginning 0) (match-end 0))) '<))
      )
      (when
        ;; line
        (progn
          ;; searching "line ..." in current line 
          (goto-char bol)
          (setq res (search-forward-regexp "line [0-9]+" nil t))
          (and res (<= res eol))
        )
        (search-backward-regexp " [0-9 ]+" nil t)
        (setq lines (cons (read (buffer-substring-no-properties (match-beginning 0) (match-end 0))) nil))
      )
    )
    (skill-open-file file lines)
  )
)

;; =================================================================================
;; Creating skill-log-mode
;; =================================================================================

(defun skill-log-mode nil
  (interactive)
  ;; deleting local variables in buffer
  (kill-all-local-variables)
  ;; using skill-log-mode keywords and removing string and comments from the syntax detection
  (set (make-local-variable 'font-lock-defaults) '(skill-log-font-lock t))
  ;; enabling buttons (clickable links)
  (button-lock-mode)
  
  ;; Error buttons
  (button-lock-set-button 
    "^.+\\*Error\\*.+line [0-9]+.*$"
    'skill-log-click-line
    :face 'user-method-face :mouse-face 'user-keyword-face
  )
  
  ;; Lint Buttons ---
  
  ;; ERROR buttons
  (button-lock-set-button 
    "^.+ERROR.+line [0-9]+.*$" 
    'skill-log-click-line
    :face 'user-method-face :mouse-face 'user-keyword-face
  )
  (button-lock-set-button 
    "^.+ERROR.+lines ([0-9 ]+).*$" 
    'skill-log-click-line
    :face 'user-method-face :mouse-face 'user-keyword-face
  )
  ;; WARN buttons
  (button-lock-set-button 
    "^.+WARN.+line [0-9]+.*$" 
    'skill-log-click-line
    :face 'user-comment-face :mouse-face 'user-keyword-face
  )
  (button-lock-set-button 
    "^.+WARN.+lines ([0-9 ]+).*$" 
    'skill-log-click-line
    :face 'user-comment-face :mouse-face 'user-keyword-face
  )
  ;; INFO buttons
  (button-lock-set-button 
    "^.+INFO.+line [0-9]+.*$" 
    'skill-log-click-line
    :face 'user-symbol-face :mouse-face 'user-keyword-face
  )
  (button-lock-set-button 
    "^.+INFO.+lines ([0-9 ]+).*$" 
    'skill-log-click-line
    :face 'user-symbol-face :mouse-face 'user-keyword-face
  )
  ;; SUGGEST buttons
  (button-lock-set-button 
    "^.+SUGGEST.+line [0-9]+.*$" 
    'skill-log-click-line
    :face 'user-symbol-face :mouse-face 'user-keyword-face
  )
  (button-lock-set-button 
    "^.+SUGGEST.+lines ([0-9 ]+).*$" 
    'skill-log-click-line
    :face 'user-symbol-face :mouse-face 'user-keyword-face
  )
  ;; HINT buttons
  (button-lock-set-button 
    "^.+HINT.+line [0-9]+.*$" 
    'skill-log-click-line
    :face 'user-string-face :mouse-face 'user-keyword-face
  )
  (button-lock-set-button 
    "^.+HINT.+lines ([0-9 ]+).*$"
    'skill-log-click-line
    :face 'user-string-face :mouse-face 'user-keyword-face
  )
  ;; Loading file buttons
  (button-lock-set-button 
    "^.*\\(~\\|/\\)+[a-zA-Z0-9/\\_\\-\\.]*\\.ils?.*$"
    'skill-log-click-line
    :face 'user-variable-face :mouse-face 'user-keyword-face
  )
  
  ;; making skill-log-mode
  (setq major-mode 'skill-log-mode)
  (setq mode-name "skill-log")
  (run-mode-hooks 'skill-log-hooks)
  
  ;; enabling auto-revert
  (auto-revert-tail-mode t)
  
);defun

(provide 'skill-log)








