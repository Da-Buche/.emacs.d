;; =============================================================================================================
;; Smart Deleting using delete, backspace or backtab
;; 
;; Author: Aurélien Buchet
;; =============================================================================================================

;; Deleting backward and forward
(defun global-delete-backward nil "Deletes backward char, if it is a space deletes all the following ones"
  (interactive)
  (if (eq (char-before) ?\ )
    (while (eq (char-before) ?\ ) (delete-backward-char 1))
    (delete-backward-char 1)
  );if
);defun

(defun global-delete-forward nil "Deletes forward char, if it is a space deletes all the following ones"
  (interactive)
  (if (eq (char-after) ?\ )
    (while (eq (char-after) ?\ ) (delete-char 1))
    (delete-char 1)
  );if
);defun

(defun global-delete-extra-spaces nil "Deletes all extra spaces in current line" (interactive)
  (save-excursion (beginning-of-line)
    (while (not (eolp))
      (while (and (eq (char-after) ?\s)
                  (or (member (char-after (+ (point) 1)) '(?\) ?\s)) (= (char-before) ?\()))
        (delete-region (point) (+ (point) 1)))
      (forward-char))
  );save-excursion
);defun

(global-set-key (kbd "<backtab>")
  (lambda nil (interactive) (global-delete-extra-spaces) (indent-according-to-mode)))

(global-set-key (kbd "<backspace>") 'global-delete-backward)
(global-set-key (kbd "C-d")         'global-delete-forward)
(global-set-key (kbd "<delete>")    'global-delete-forward)












