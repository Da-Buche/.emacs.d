(require 'htmlfontify)

(defun color-name-to-html (color-name)
  "Convert COLOR-NAME to its HTML hexadecimal representation."
  (cl-destructuring-bind (r g b) (color-name-to-rgb color-name)
    (format "#%02x%02x%02x" (* r 255) (* g 255) (* b 255))
      (error (format "Color name `%s' not recognized" color-name))))

(defun htmlfontify-string (string)
  "Take a STRING and return a fontified version of it.
It is assumed that STRING has text properties that allow it to be
fontified.  This is a simple convenience wrapper around
`htmlfontify-buffer'."
  (let* ( ( hfy-optimizations-1 (copy-sequence hfy-optimizations)                    )
          ( hfy-optimizations (cl-pushnew 'skip-refontification hfy-optimizations-1) )
          )
    (with-temp-buffer
      (insert string)
      (htmlfontify-buffer)
      (prog1 (buffer-string) (kill-buffer (current-buffer)))
      )))

(defun htmlfontify-region-or-buffer-to-clipboard ()
  "Htmlize the current buffer and return the body only."
  (interactive)
  ;; Fetch current region (or the whole buffer) substring
  (let* ( ( beg (if (region-active-p) (region-beginning) (point-min)) )
          ( end (if (region-active-p) (region-end)       (point-max)) )
          ( str (buffer-substring beg end)                            )
          )
    ;; Generate HTML output in temporary buffer
    (with-temp-buffer
      (insert (htmlfontify-string str))
      ;; Keep only HTML <body> part
      (beginning-of-buffer)
      (search-forward "<body")
      (delete-region (point-min) (match-beginning 0))
      (search-forward "</body>")
      (delete-region (point) (point-max))
      ;; Set font background and foreground
      (beginning-of-buffer)
      (when (search-forward "<pre>")
        (replace-match "<pre style='background-color:#262626; color:#faf0e6;'>"))
      ;; Replace <span> classes by their actual color
      (beginning-of-buffer)
      (while (search-forward-regexp "<span class=\"\\([a-zA-Z-]+\\)\">" nil t)
        (cl-destructuring-bind (r g b) (color-name-to-rgb (face-foreground (intern (match-string 1))))
          (replace-match (concat "<span style='color:" (format "#%02x%02x%02x" (* r 255) (* g 255) (* b 255)) "'>"))))
      ;; Copy the generated string to clipboard and kill temp buffer
      (kill-new (buffer-string))
      (kill-buffer (current-buffer))
      )))

;; (format "rgb(%f,%f,%f)" r g b)

