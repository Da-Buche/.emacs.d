;; Global functions useful in a lot of cases

(defun replace (list n elt)
  "replaces the nth element of the list with elt and returns the list"
  (cond
    ((equal list nil) nil)
    ((equal n 0) (cons elt (cdr list)))
    (t (cons (car list) (replace (cdr list) (- n 1) elt)))
  );cond
);defun

(defun sum (&rest args)
  "returns the sum of the list in argument, t are considered as 1, nil as 0"
  (let (
      (head (car args))
      (tail (cdr args))
    )
    (cond
      ;; if there is no tail, returning head value
      ((not tail)
        (cond
          ;; head is t, returning one
          ((eq head t) 1)
          ;; returning head if it is a number
          (head)
          ;; returning 0 it it is nil
          (0)
        );cond
      )
      ;; if head is nil, just neglects it
      ((not head) (apply 'sum tail))
      ;; if head is t, adds one to the result
      ((eq head t) (+ 1 (apply 'sum tail)))
      (t (+ head (apply 'sum tail)))
    );cond
  );let
);defun

(defun nthcar (n list)
  "returns the n first elements of a list"
  (cond
    ((equal list nil) nil)
    ((< n 1) nil)
    ((cons (pop list) (nthcar (- n 1) list)))
  );cond
);defun

(defun sublist (list x y)
  "returns the sublist of list from the x element to the y element"
  (cond
    ((equal list nil) nil)
    ((equal y 0) (cons (pop list) nil))
    ((< y 0) nil)
    ((< x 1) (cons (pop list) (sublist list 0 (- y 1))))
    (t (sublist (cdr list) (- x 1) (- y 1)))
  );cond
);defun

(defmacro case (elt &rest args)
  "takes an element and an undefined number of lists in argument
  if the argument corresponds to the car of a list, it returns its cdr"
  `(let* (
      (tail (car ,args))
      (head (pop tail))
    )
    (if (or (eq head t) (equal ,elt head))
      (if tail
        tail
        head
      );if
      (case ,elt (caar ,args))
    );if
  );let*
);defmacro
