(require 'subr-x)

;; Unless-error to assert

(defun aira--unless-error-to-assert ()
  "Convert: unless(expr error(\"msg\")) -> assert(expr \"msg\")"
  (interactive)
  (when (re-search-forward
         "unless(\\([^)]*\\)[ \t\n]*error(\\(\s*\"[^\"]\"*[^)]*\\)))"
         nil t)
    (replace-match "assert(\\1 \\2)" t nil)))

(defun aira-unless-error-to-assert ()
  "Run once, then repeat with C-x e."
  (interactive)
  (setq last-kbd-macro
        (kbd "M-x aira--unless-error-to-assert RET"))
  (aira-docify-next-procedure))

;; Comments above procedures to docstrings

(defun aira--escape-skill-string (s)
  ;(setq s (replace-regexp-in-string "\\" "\\\\" s t t))
  (setq s (replace-regexp-in-string "\"" "\\\"" s t t))
  s)

(defun aira-docify-next-procedure ()
  "Convert contiguous comment lines immediately above the next procedure into a docstring."
  (interactive)
  (when (re-search-forward "^[ \t]*procedure[ \t]*(" nil t)
    (beginning-of-line)

    (let (comments
          comment-start
          comment-end)

      (save-excursion
        (forward-line -1)

        (when (looking-at "^[ \t]*;+")
          (setq comment-end (line-end-position))

          (while (looking-at "^[ \t]*;+")
            (setq comment-start (line-beginning-position))
            (setq comments
                  (cons
                   (replace-regexp-in-string
                    "^[ \t]*;+[ \t]?"
                    ""
                    (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position)))
                   comments))
            (if (bobp)
                (goto-char (point-min))
              (forward-line -1)))))

      (when comments
        (delete-region comment-start (min (point-max) (1+ comment-end)))

        (goto-char comment-start)
        (end-of-line)

        (insert
         "\n  \""
         (aira--escape-skill-string
          (string-join comments "\n"))
         "\"")))))

(defun aira-docify-start ()
  "Run once, then repeat with C-x e."
  (interactive)
  (setq last-kbd-macro
        (kbd "M-x aira-docify-next-procedure RET"))
  (aira-docify-next-procedure))
