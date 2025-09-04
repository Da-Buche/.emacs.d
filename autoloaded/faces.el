;; =============================================================================================================
;; Default Font, Face and Background
;; =============================================================================================================

;; =================================================================================
;; Utilities to get and set current font size
;; =================================================================================

(defun current-font-size nil
  "Return current font size"
  ;; 1 pixel (px) is usually assumed to be 1/96th of an inch
  ;; 1 point (pt) is assumed to be 1/72nd of an inch.
  ;; Therefore 4px = 3pt.
  (/ (* 3 (elt (font-info (face-attribute 'default :font)) 2)) 4))

(defun get-ideal-frame-font-size (&optional cols)
  "Set the font size of the current frame so that it contains at least COLS (defaulting to 130) columns."
  ;; Emacs 'DejaVu Sans Mono' font size/width ratio is 5/4
  (let* ((window-width  (/ (frame-pixel-width) 2.0)   ) ;(window-pixel-width)
         (desired-cols  (+ (or cols 135) 5)           )
         (char-width    (/ window-width desired-cols) )
         )
    (round (* char-width 1.25))
    ))

(defun set-frame-font-size (&optional n)
  "Set frame font size to N"
  (interactive (let ((size       (format "%d" (current-font-size)))
                     (ideal-size (format "%d" (get-ideal-frame-font-size)))
                     )
                 (list (read-string (format "Font Size (%s): " size) ideal-size nil ideal-size))))
  (let ((size (or n "12")))
    (set-frame-font (format "DejaVu Sans Mono-%s" (or n "12")) t)
    size
    ))

;; Set smaller default face
(if (equal (system-name) "Aureliens-MacBook-Pro.local")
    (set-frame-font-size 12)
  (set-frame-font-size 10)
  )

;; =================================================================================
;; Defining User Faces
;; =================================================================================

;; basic text
(defface user-basic-face
  '((default :foreground "linen"))
  "Face of basic text"
  :group 'user-faces)

;; symbols
(defface user-symbol-face
  '((default :foreground "rosy brown"))
  "Face of symbols"
  :group 'user-faces)

;; strings
(defface user-string-face
  '((default :foreground "dark salmon"))
  "Face of strings"
  :group 'user-faces)

;; comments
(defface user-comment-face
  '((default :foreground "dark orange"))
  "Face of comments"
  :group 'user-faces)

;; forms
(defface user-form-face
  '((default :foreground "light steel blue"))
  "Face of inherent language forms"
  :group 'user-faces)

;; functions
(defface user-function-face
  '((default :foreground "Deep sky blue"))
  "Face of user defined functions"
  :group 'user-faces)

;; variables
(defface user-variable-face
  '((default :foreground "medium sea green"))
  "Face of variables"
  :group 'user-faces)

;; keywords
(defface user-keyword-face
  '((default :foreground "plum"))
  "Face of keywords"
  :group 'user-faces)

;; classes
(defface user-class-face
  '((default :foreground "light goldenrod"))
  "Face of classes"
  :group 'user-faces)

;; methods
(defface user-method-face
  '((default :foreground "indian red"))
  "Face of symbols"
  :group 'user-faces)

