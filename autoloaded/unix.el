
;; Window title is Emacs <buffer-file-name>
(setq
 frame-title-format
 '( "Emacs "
    (:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b"))
    (:eval (if (buffer-modified-p) " *"))
    ))

