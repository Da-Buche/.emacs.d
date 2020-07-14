;; ===============================================================================================================
;; Personal user bindkeys
;; 
;; Author: Aurelien Buchet
;; ===============================================================================================================

;; -------------------------------------------------------
;; Backward kill line
;; Same C-u as terminal
;;
;; from:
;; https://www.emacswiki.org/emacs/BackwardKillLine
;; 12/08/2019
;; -------------------------------------------------------
(defun backward-kill-line (arg) "Kill ARG lines backward." (interactive "p") (kill-line (- 1 arg)))

(global-set-key (kbd "C-u") 'backward-kill-line)

;; -------------------------------------------------------
;; Unbind Pesky Sleep Button
;; -------------------------------------------------------
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])

;; -------------------------------------------------------
;; Mixed-case
;; -------------------------------------------------------
(defun mixcase-word nil (interactive)
  (let (beg end)
    ;; Fetching beginning and end of next word
    (right-word) (left-word) (setq beg (point)) (right-word) (setq end (point))
    (upcase-initials-region beg end)))

(global-set-key (kbd "M-m") 'mixcase-word)

;; -------------------------------------------------------
;; Placing point at beginning of a line
;; when clicking on line number
;; -------------------------------------------------------
(defun global-click-line (click)
  (interactive "e")
  ;; If point is at the beginning of a line sets the mark,
  ;; otherwise sets the point at the beginning of the clicked line
  (if (not (bolp)) (progn (deactivate-mark) (mouse-set-point click))
    (progn (push-mark) (activate-mark) (mouse-set-point click) (end-of-line))))

(global-set-key (kbd "<left-margin> <mouse-1>") 'global-click-line)

;; -------------------------------------------------------
;; Find file
;; -------------------------------------------------------
;; Remove C-x f shortkey
(global-set-key (kbd "C-x f") 'find-file-no-remap)
(global-set-key (kbd "C-x C-f") 'find-file)

;; Sexp management

(defun forward-copy-sexp nil
  (interactive)
  (forward-kill-sexp)
  (undo))

(global-set-key (kbd "<C-M-w>"        ) 'forward-copy-sexp)
(global-set-key (kbd "<C-M-backspace>") 'backward-kill-sexp)
(global-set-key (kbd "<C-M-return>"   ) 'newline)





;; find-file with new name so that ido does not remap it
(defun find-file-no-remap (filename &optional wildcards)
  "Edit file FILENAME.
Switch to a buffer visiting file FILENAME,
creating one if none already exists.
Interactively, the default if you just type RET is the current directory,
but the visited file name is available through the minibuffer history:
type M-n to pull it into the minibuffer.

You can visit files on remote machines by specifying something
like /ssh:SOME_REMOTE_MACHINE:FILE for the file name.  You can
also visit local files as a different user by specifying
/sudo::FILE for the file name.
See the Info node `(tramp)File name Syntax' in the Tramp Info
manual, for more about this.

Interactively, or if WILDCARDS is non-nil in a call from Lisp,
expand wildcards (if any) and visit multiple files.  You can
suppress wildcard expansion by setting `find-file-wildcards' to nil.

To visit a file without any kind of conversion and without
automatically choosing a major mode, use \\[find-file-literally]."
  (interactive
   (find-file-read-args "Find file: "
                        (confirm-nonexistent-file-or-buffer)))
  (let ((value (find-file-noselect filename nil nil wildcards)))
    (if (listp value)
	(mapcar 'switch-to-buffer (nreverse value))
      (switch-to-buffer value))))



