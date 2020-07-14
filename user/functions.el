;; =============================================================================================================
;; Global functions useful in a lot of cases
;; 
;; Author: Aurelien Buchet
;; =============================================================================================================

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
  (let ((head (car args))
        (tail (cdr args)))
    (cond
      ;; if there is no tail, returning head value
      ;; return 1 for t, number if it is one, 0 otherwise
      ((not tail) (cond ((eq head t) 1) (head) (0)))
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

(defun strcat (&rest args)
  (let ((res ""))
    (dolist (arg args)
      (cond 
        ((symbolp arg) (setq res (concat res (symbol-name arg))))
        ((stringp arg) (setq res (concat res arg)))
      (t (error "strcat: argument %s is not a string" arg)));cond
    );dolist
    res));let ;defun

(defun symcat (&rest args)
  (intern (apply 'strcat args)))

(defmacro plist-push (plist prop val)
  "Like plist-put but modifies the plist, also merges strings into symbols"
  `(setq ,plist (plist-put ,plist (symcat ,prop) ,val)))

;; -------------------------------------------------------------------------------------------------------------
;; @fun plist-val
;;
;; @doc Allows chain plist-gets using multiple properties at the same time, also merges strings into symbols
;;
;; @arg plist
;; @type list
;; @doc The given property list to extract properties
;;
;; @arg args
;; @type list
;; @def rest
;; @doc The list describing the property to retrieve by chaining plist-get
;; 
;; @example
;; (setq plist (list 'name "property list" 'properties '(a "prop a" b "prop b")))
;; 
;; (plist-val plist 'name)
;; => "property list"
;; 
;; (plist-val plist 'properties)
;; => (a "prop a" b "prop b")
;; 
;; (plist-val plist 'properties 'a)
;; => "prop a"
;; -------------------------------------------------------------------------------------------------------------
(defun plist-val (
    plist ;property list ;property list from which the property defined by args will be extracted if it exists
    &rest args ;symbol list ;can contain an unlimited number of symbols describing a property to extract in property list hierarchy
  )
  "Allows the use of plist-get with multiple arguments"
  (let ((res plist))
    ;; Chaining plist-get
    (dolist (arg args) (setq res (plist-get res (symcat arg))))
    ;; Returning 
    res))  

(defmacro check-string (symbol)
  "Checks if symbol is bound to a string and if it's not associates "" to the symbol"
  `(if (and (boundp ',symbol) ,symbol (stringp ,symbol))
    ,symbol (setq ,symbol "")))




