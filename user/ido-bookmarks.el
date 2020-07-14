;; ---------------------------------------------------------------------------------------------------------------
;; Use ido with bookmarks
;; 
;; Merge of hints and anwsers below
;; ---------------------------------------------------------------------------------------------------------------
(require 'ido)
(require 'bookmark)

;; Ido go to bookmark
(define-key ctl-x-r-map "b" 'ido-goto-bookmark)

(defun ido-goto-bookmark nil (interactive)
       (bookmark-jump (ido-completing-read "Jump to bookmark: " (bookmark-all-names))))

;; Ido go to bookmark other window
(global-unset-key (kbd "C-x 4 r"))
(defvar ctl-x-4-r-map (make-sparse-keymap) "Keymap for subcommands of C-x 4 r.")
(define-key ctl-x-4-map "r" ctl-x-4-r-map)
(define-key ctl-x-4-r-map "b" 'ido-goto-bookmark-other-window)

(defun ido-goto-bookmark-other-window nil (interactive)
       (bookmark-jump-other-window (ido-completing-read "Jump to bookmark: " (bookmark-all-names))))

;; ---------------------------------------------------------------------------------------------------------------
;; From https://www.emacswiki.org/emacs/InteractivelyDoThings#toc11
;; 08/08/19
;;
;; Invoking Bookmarks From Ido
;; 
;; Did you ever want to use bookmarks from within ido?
;; I just did a little mashup of bookmark and ido code, just M-C-b from your ido file selection.
;; -- AnselmHelbig
;; ---------------------------------------------------------------------------------------------------------------

;(setq enable-recursive-minibuffers t)
;(define-key ido-file-dir-completion-map [(meta control ?b)] 'ido-goto-bookmark)
;
;(defun ido-goto-bookmark (bookmark)
;  (interactive
;   (list (bookmark-completing-read "Jump to bookmark"
;    				   bookmark-current-bookmark)))
;  (unless bookmark
;    (error "No bookmark specified"))
;  (let ((filename (bookmark-get-filename bookmark)))
;    (ido-set-current-directory
;     (if (file-directory-p filename)
;         filename
;       (file-name-directory filename)))
;    (setq ido-exit        'refresh
;          ido-text-init   ido-text
;          ido-rotate-temp t)
;    (exit-minibuffer)))

;; ---------------------------------------------------------------------------------------------------------------
;; From https://stackoverflow.com/questions/19378278/completion-choices-for-bookmarks-gone-from-emacs-minibuffer
;; 08/08/19
;;
;; Try this
;; 
;; answered Oct 19 '13 at 6:58 
;; -- artscan
;; ---------------------------------------------------------------------------------------------------------------
;(global-set-key (kbd "C-x r b")
;    (lambda ()
;      (interactive)
;      (bookmark-jump
;       (ido-completing-read "Jump to bookmark: " (bookmark-all-names)))))







                
