;; From https://www.emacswiki.org/emacs/IncrementNumber on 2026-07-15

(defun increment-number-at-point (&optional increment)
  "Increment the number at point by INCREMENT."
  (interactive "*p")
  (let ((pos (point)))
    (save-match-data
      (skip-chars-backward "0-9")
      (if (looking-at "[0-9]+")
          (let ((field-width (- (match-end 0) (match-beginning 0)))
                (newval (+ (string-to-number (match-string 0) 10) increment)))
            (when (< newval 0)
              (setq newval (+ (expt 10 field-width) newval)))
            (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                   newval)))
        (user-error "No number at point")))
    (goto-char pos)))

(defun decrement-number-at-point (&optional decrement)
  "Decrement the number at point by DECREMENT."
  (interactive "*p")
  (increment-number-at-point (- decrement)))

(global-set-key (kbd "C-c   +") 'increment-number-at-point)
(global-set-key (kbd "C-c C-+") 'increment-number-at-point)
(global-set-key (kbd "C-c   -") 'decrement-number-at-point)
(global-set-key (kbd "C-c C--") 'decrement-number-at-point)

