;; =============================================================================================================
;; Useful functions and macro to ease emacs lisp scripting
;; 
;; Author: Aurélien Buchet
;; =============================================================================================================

(defun fnil (&rest args) "Returns nil whatever the arguments" nil)

(defmacro for (var start end &rest body) "Classic for loop"
  `(let ((,var ,start)) (while (< ,var ,end) ,@body (setq ,var (+ ,var 1)))))

(defun divisors (n) "Returns the list of divisors of the given integer"
  (let (l_divisors)
    (for i 1 (ceiling (sqrt n)) (when (zerop (mod n i)) (push (cons i (/ n i)) l_divisors)))
    l_divisors))

(defun str-times (n str) "Multiplies a string using concatenation"
  (let ((res "")) (for i 0 n (setq res (concat res str))) res))

