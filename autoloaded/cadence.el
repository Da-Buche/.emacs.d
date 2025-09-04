(require 'htmlfontify)

(defun color-name-to-html (color-name)
  "Convert COLOR-NAME to its HTML hexadecimal representation."
  (cl-destructuring-bind (r g b) (color-name-to-rgb color-name)
    (format "#%02x%02x%02x" (* r 255) (* g 255) (* b 255))
      (error (format "Color name `%s' not recognized" color-name))))

(defun htmlfontify-region-or-buffer-to-clipboard ()
  "Htmlize the current buffer and return the body only."
  (interactive)
  ;; Fetch current region (or the whole buffer) substring
  (let* ((beg (if (region-active-p) (region-beginning) (point-min)))
         (end (if (region-active-p) (region-end)       (point-max)))
         (str (buffer-substring beg end))
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
      ;; Copy the generated string to clipboard
      (kill-new (buffer-string))
      )))

;; (format "rgb(%f,%f,%f)" r g b)

