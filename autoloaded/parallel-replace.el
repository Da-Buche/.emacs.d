(require 'cl)
(defun parallel-replace (plist &optional delimited start end)
  "Replace every occurrence of the (2n)th token of PLIST in
buffer with the (2n+1)th token; if only two tokens are provided,
replace them with each other (ie, swap them).

If optional second argument DELIMITED is nil, match words
according to syntax-table; otherwise match symbols.

When called interactively, PLIST is input as space separated
tokens, and DELIMITED as prefix arg."
  (interactive
   `(,(loop with input = (read-from-minibuffer "Replace: ")
            with limit = (length input)
            for  j = 0 then i
            for (item . i) = (read-from-string input j)
            collect (prin1-to-string item t) until (<= limit i))
     ,current-prefix-arg
     ,@(if (use-region-p) `(,(region-beginning) ,(region-end)))))
  (let* ((alist (cond ((= (length plist) 2) (list plist (reverse plist)))
                      ((loop for (key val . tail) on plist by #'cddr
                             collect (list (prin1-to-string key t) val)))))
         (matcher (regexp-opt (mapcar #'car alist)
                              (if delimited 'words 'symbols)))
         (to-spec `(replace-eval-replacement replace-quote
                    (cadr (assoc-string (match-string 0) ',alist
                                        case-fold-search)))))
    (query-replace-regexp matcher to-spec nil start end)))

