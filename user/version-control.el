;; ===============================================================================================================
;; Emacs version control setup
;; 
;; A. Buchet 
;; ===============================================================================================================

(require 'vc-hooks)

;; Always follow version controlled symbolic lonks
(setq-default vc-follow-symlinks t)

(defun vc-git-status (path)
  "Return git shorten status string at PATH"
  (unless (file-directory-p path) (setq path (file-name-directory path)))
  (shell-command-to-string (format "cd %s ; git fetch ; git status -sb" path)))

(defun vc-git-forward-commits (path)
  "Return the number of forward commits at PATH"
  (let ((str (car (split-string (vc-git-status path) "\n")))
        res)
    (setq res (cadr (split-string str "\\(\\[behind\\|\\]\\)" t)))
    (if res (string-to-number res)
      (setq res (cadr (split-string str "\\(\\[forward\\|\\]\\)" t)))
      (if res (- (string-to-number res))
        0))))

(defun vc-git-need-update (path)
  "Return t if file at full PATH is concerned by a forward commit"
  (not (eq 1
           (shell-command
            (format
             "\
cd %s ; \
git fetch ; \
git status -sb | \
grep -Po '(?<=\\[behind )[0-9]+' | \
xargs -I@ git log -n @ --pretty=format:\"\" --name-only | \
grep -q %s"
             (file-name-directory path)
             (file-name-nondirectory path)
             )))))

;(defun vc-update-opened-file (&rest args)
;  "Check if opened file needs update and try to update it"
;   (let ((file (buffer-file-name)))
;; Git fetch, check behind commits, list concerned files
;     file))

;; (add-hook 'find-file-hook 'vc-update-opened-file)

