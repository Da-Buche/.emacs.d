;; minesweeper.el --- implementation of Minesweeper for Emacs
;;
;; Author Aurélien Buchet <au.buchet@gmail.com>


;; =================================================================================
;; To do list
;; =================================================================================

;; When changing case status, deletes the char if it is not a '.'
;; Explosion when losing

;; =================================================================================
;; Variables
;; =================================================================================

(defvar minesweeper-width 39
  "Width of playing area."
);defvar

(defvar minesweeper-height 19
  "Height of playing area"
);defvar

(defvar minesweeper-mines 109
  "Number of mines"
);defvar

(defvar minesweeper-revealed-squares 0
  "Number of cases revealed by the player"
);defvar

(defvar minesweeper-matrix nil
  "The matrix corresponding to the grid"
);defvar

(defvar minesweeper-beginning-time (cadr (current-time))
  "The beginning time of the game"
);defvar

;; =================================================================================
;; Constants
;; =================================================================================

;; Buffer name
(defconst minesweeper-buffer-name "*Minesweeper*")

;; =================================================================================
;; Group, Hook and Keymap
;; =================================================================================

(defgroup minesweeper nil
  "Minesweeper"
  :prefix "minesweeper-"
  :group 'games
);defgroup

(defcustom minesweeper-mode-hook nil
  "If non-nil, its value is called on entry to minesweeper mode.
One useful value to include is `turn-on-font-lock' to highlight the pieces."
  :type 'hook
  :group 'minesweeper
);defcustom

(defvar minesweeper-mode-map
  (let ((map (make-sparse-keymap 'minesweeper-mode-map)))
    ;; option keys
    (define-key map "r"          'minesweeper-restart-game)
    ;; moving keys
    (define-key map [up]         'minesweeper-move-up)
    (define-key map [down]       'minesweeper-move-down)
    (define-key map [left]       'minesweeper-move-left)
    (define-key map [right]      'minesweeper-move-right)
    ;; minesweeping keys
    (define-key map [return]     'minesweeper-reveal-square)
    (define-key map [backspace]  'minesweeper-change-square-state)
    ;; mouse keys
    (define-key map [mouse-1]    'minesweeper-left-click)
    (define-key map [mouse-3]    'minesweeper-right-click)
    ;; returns the map
    map
  );let
  "local keymap to use in Minesweeper mode"
);defvar

;; =================================================================================
;; Faces and Font-Lock
;; =================================================================================

(defface minesweeper-is-safe-face
  '((((class color)) (:foreground "green1" :weight bold)))
  "face to use for supposed safe zone"
  :group 'minesweeper 
);defface

(defface minesweeper-is-mined-face
  '((((class color)) (:foreground "red1" :weight bold)))
  "face to use for supposed mined zone"
  :group 'minesweeper
);defface

(defface minesweeper-mine-face
  '((((class color)) (:foreground "red4" :weight bold)))
  "face to use for mines"
  :group 'minesweeper
);defface

(defface minesweeper-zero-face
  '((((class color)) (:foreground "green4" :weight bold)))
  "face to use for zero mine around"
  :group 'minesweeper
);defface

(defface minesweeper-one-face
  '((((class color)) (:foreground "cyan1" :weight bold)))
  "face to use for one mine around"
  :group 'minesweeper
);defface

(defface minesweeper-two-face
  '((((class color)) (:foreground "cyan2" :weight bold)))
  "face to use for two mines around"
  :group 'minesweeper
);defface

(defface minesweeper-three-face
  '((((class color)) (:foreground "cyan3" :weight bold)))
  "face to use for three mines around"
  :group 'minesweeper
);defface

(defface minesweeper-four-face
  '((((class color)) (:foreground "cyan4" :weight bold)))
  "face to use for two four around"
  :group 'minesweeper
);defface

(defface minesweeper-five-face
  '((((class color)) (:foreground "blue1" :weight bold)))
  "face to use for five mines around"
  :group 'minesweeper
);defface

(defface minesweeper-six-face
  '((((class color)) (:foreground "blue2" :weight bold)))
  "face to use for six mines around"
  :group 'minesweeper
);defface

(defface minesweeper-seven-face
  '((((class color)) (:foreground "blue3" :weight bold)))
  "face to use for seven mines around"
  :group 'minesweeper
);defface

(defface minesweeper-eight-face
  '((((class color)) (:foreground "blue4" :weight bold)))
  "face to use for eight mines around"
  :group 'minesweeper
);defface

(defvar minesweeper-font-lock-keywords
  '(
    ("!" . 'minesweeper-mine-face)
    ("?" . 'minesweeper-is-safe-face)
    ("‽" . 'minesweeper-is-mined-face)
    ("0" . 'minesweeper-zero-face)
    ("1" . 'minesweeper-one-face)
    ("2" . 'minesweeper-two-face)
    ("3" . 'minesweeper-three-face)
    ("4" . 'minesweeper-four-face)
    ("5" . 'minesweeper-five-face)
    ("6" . 'minesweeper-six-face)
    ("7" . 'minesweeper-seven-face)
    ("8" . 'minesweeper-eight-face)
  )
  "font lock rules for minesweeper"
);defvar

;; =================================================================================
;; Mode
;; =================================================================================

(define-derived-mode minesweeper-mode special-mode "Minesweeper"
  "major mode for playing minesweeper"
  (make-local-variable 'font-lock-defaults)
  (setq
    font-lock-defaults '(minesweeper-font-lock-keywords t)
    buffer-read-only t
  );setq
);define

;; =================================================================================
;; Creating the minesweeper matrix
;; =================================================================================

(defun minesweeper-create-matrix nil
  "returns a matrix of minesweeper-height * minesweeper-width squares"
  (make-list minesweeper-height (make-list minesweeper-width nil))
);defun

(defun minesweeper-mines-position nil
  "returns a list of random mines position
  the length of the list is minesweeper-mines"
  (let (position)
    ;; creating a list of all positions
    (setq position
      (dotimes (i (* minesweeper-height minesweeper-width) position)
        (setq position (cons i position))
      );dotimes
    );setq
    ;; deleting random positions until the position list correspond to the number of mines
    (while (> (length position) minesweeper-mines)
      (let ((rand (random (length position))))
        (setq position (append (butlast position (+ rand 1)) (last position rand))
        );setq
      );let
    );while
    position
  );let
);defun

(defun minesweeper-place-mine (matrix position)
  "places a mine in the matrix and returns it"
  (let ((x (/ position minesweeper-width))
      (y (% position minesweeper-width)))
    (replace matrix x (replace (nth x matrix) y t))
  );let
);defun

(defun minesweeper-place-mines (matrix mines-position)
  "places all the mines of the mines-position list into the matrix and returns it"
  (if mines-position
    (minesweeper-place-mines 
      (minesweeper-place-mine matrix (car mines-position))
      (cdr mines-position)
    )
    matrix
  );if
);defun

(defun minesweeper-init-matrix nil
  "creates minesweeper-matrix and places minesweeper-mines inside"
  (setq minesweeper-matrix
    (minesweeper-place-mines (minesweeper-create-matrix) (minesweeper-mines-position))
  );setq
);defun

;; =================================================================================
;; Getting the number of mines around a square
;; =================================================================================

(defun minesweeper-is-square-mined (x y)
  "takes the position of a square in argument and returns true if it is mined, nil otherwise"
  (nth y (nth x minesweeper-matrix))
);defun

(defun minesweeper-get-neighboring-squares (matrix x y)
  "returns the matrix of neighbors of the x y square in matrix"
  (mapcar (lambda (list) (sublist list (- y 1) (+ y 1)))
      (sublist matrix (- x 1) (+ x 1))
  );mapcar 
);defun

(defun minesweeper-count-mines-around (matrix x y)
  "count the number of mines around the x y square"
  (sum
    (apply 'sum 
      (apply 'append (minesweeper-get-neighboring-squares matrix x y))
    );apply
    (minesweeper-is-square-mined x y)
  );sum
);defun

;; =================================================================================
;; Dysplaying the game matrix
;; =================================================================================

(defun minesweeper-init-display (x y)
  "display an x y matrix containing . where squares are unidentified"
  (buffer-disable-undo (current-buffer))
  (let ((inhibit-read-only t))
    ;; deleting buffer
    (delete-region (point-min) (point-max))
    ;; displaying a matrix of .
    (dotimes (i x)
      (dotimes (j y)
        (insert ".   ")
      );dotimes y
      (insert "\n\n")
    );dotimes x
  );let
);defun

;; =================================================================================
;; Moves
;; =================================================================================

(defun minesweeper-point-position nil
  "returns (x y) value of the point position in minesweeper-matrix"
  (cons (round (/ (float (- (line-number-at-pos) 1)) 2)) (round (/ (float (current-column)) 4)))
);defun

(defun minesweeper-move-to (x y)
  "places the point to the x y square"
  (goto-char (+ (* x (+ (* minesweeper-width 4) 2)) (* y 4) 1))
)

(defun minesweeper-move-up nil
  "moves the point to the square up to it"
  (interactive)
  (let* (
      (inhibit-read-only t)
      (y (minesweeper-point-position))
      (x (pop y))
    )
    (if (equal x 0)
      (minesweeper-move-to (- minesweeper-height 1) y)
      (minesweeper-move-to (- x 1) y)
    );if
  );let*
);defun

(defun minesweeper-move-down nil
  "moves the point to the square down to it"
  (interactive)
  (let* (
      (inhibit-read-only t)
      (y (minesweeper-point-position))
      (x (pop y))
    )
    (if (equal x (- minesweeper-height 1))
      (minesweeper-move-to 0 y)
      (minesweeper-move-to (+ x 1) y)
    );if
  );let*
);defun

(defun minesweeper-move-left nil
  "moves the point to the square left to it"
  (interactive)
  (let* (
      (inhibit-read-only t)
      (y (minesweeper-point-position))
      (x (pop y))
    )
    (if (equal y 0)
      (minesweeper-move-to x (- minesweeper-width 1))
      (minesweeper-move-to x (- y 1))
    );if
  );let*
);defun

(defun minesweeper-move-right nil
  "moves the point to the square right to it"
  (interactive)
  (let* (
      (inhibit-read-only t)
      (y (minesweeper-point-position))
      (x (pop y))
    )
    (if (equal y (- minesweeper-width 1))
      (minesweeper-move-to x 0)
      (minesweeper-move-to x (+ y 1))
    );if
  );let*
);defun

;; =================================================================================
;; Actions to play
;; =================================================================================

(defun minesweeper-reveal-square (&optional x y)
  "reveals the clicked square"
  (interactive)
  (when (and x y) (minesweeper-move-to x y))
  (save-excursion
    (let* (
        (inhibit-read-only t)
        (y (minesweeper-point-position))
        (x (pop y))
        (char (char-after))
        safe-around-squares
      )
      (unless (and (<= ?0 char) (< char ?9))
        ;; deleting the "." or "?" or "‽"
        (delete-forward-char 1)
        ;; displaying X if square is mined or displays the number of mines around
        (if (minesweeper-is-square-mined x y)
          (minesweeper-explosion)
          (progn
            (insert (setq char (+ ?0 (minesweeper-count-mines-around minesweeper-matrix x y))))
            (setq minesweeper-revealed-squares (+ minesweeper-revealed-squares 1))
            (when (= minesweeper-revealed-squares (- (* minesweeper-height minesweeper-width) minesweeper-mines))
              (insert "You Fuckin' Win Boy !")
            )
          );progn
        );if 
        (backward-char)
      );unless
      (when (setq safe-around-squares (minesweeper-safe-squares-around x y))
        (while (setq y (pop safe-around-squares) x (pop y))
          (minesweeper-reveal-square x y)
        );while
      );when
    );let*
  );save-excursion
);defun

(defun minesweeper-safe-squares-around (&optional x y)
  "returns the list of safe undiscovered square's positions around the (x . y) square"
  (when (and x y) (minesweeper-move-to x y))
  (let* (
      (y (minesweeper-point-position))
      (x (pop y))
      (around-squares (list 
          (cons (- x 1) (- y 1))
          (cons (- x 1) y)
          (cons (- x 1) (+ y 1))
          (cons x (- y 1))
          (cons x (+ y 1))
          (cons (+ x 1) (- y 1))
          (cons (+ x 1) y)
          (cons (+ x 1) (+ y 1))
        )
      )
      safe-around-squares
      (around-mines (- (char-after) ?0))
      char
    )
    ;; the square should contains a number
    (when (and (<= 0 around-mines) (< around-mines 9))
      ;; browsing around squares
      (while (setq y (pop around-squares) x (pop y))
        ;; The square needs to be in the grid
        (when (and
            (<= 0 x) (< x minesweeper-height)
            (<= 0 y) (< y minesweeper-width)
          );and
          (minesweeper-move-to x y)
          (setq char (char-after))
          (cond
            ((= char ?!) (setq around-mines (- around-mines 1))); (setq visited (cons (cons (cons x y) 'mine) visited))
            ((and (<= ?0 char) (< char ?9))); (setq visited (cons (cons (cons x y) 'number) visited))
            (t (setq safe-around-squares (cons (cons x y) safe-around-squares))); (setq visited (cons (cons (cons x y) 'safe) visited))
          );cond
        );when
      );while
    );when
    (when (<= around-mines 0) safe-around-squares)
  );let*
);defun

(defun minesweeper-left-click (click)
  (interactive "e")
  (minesweeper-reveal-square)
);defun

(defun minesweeper-change-square-state nil
  "change the status of the clicked square"
  (interactive)
  (let (
      (inhibit-read-only t)
      (char (char-after))
    )
    (unless (equal char ? )
      (delete-forward-char 1)
      (cond
        ((equal char ?.) (insert ?!))
        ((equal char ?!) (insert ??))
        ((equal char ??) (insert ?‽))
        ((equal char ?‽) (insert ?.))
      );cond
      (backward-char)
    );unless
  );let
);defun

(defun minesweeper-right-click (click)
  (interactive "e")
  (mouse-set-point click)
  (minesweeper-change-square-state)
);defun

;; =================================================================================
;; Losing Animation
;; =================================================================================

(defun minesweeper-circle (rayon)
  "retuns the positions of cases in the circle with current center and defined rayon"
  (save-excursion
    (let* (
        (n rayon)
        positions
      )
      ;; moving up
      (while (>= (setq n (- n 1)) 0)
        (minesweeper-move-up)
      );while
      (setq
        n rayon
        positions (cons (minesweeper-point-position) positions)
      );setq
      ;; moving right-down
      (while (>= (setq n (- n 1)) 0)
        (minesweeper-move-right)
        (minesweeper-move-down)
        (setq positions (cons (minesweeper-point-position) positions))
      );while
      (setq n rayon)
      ;; moving down-left
      (while (>= (setq n (- n 1)) 0)
        (minesweeper-move-down)
        (minesweeper-move-left)
        (setq positions (cons (minesweeper-point-position) positions))
      );while
      (setq n rayon)
      ;; moving left-up
      (while (>= (setq n (- n 1)) 0)
        (minesweeper-move-left)
        (minesweeper-move-up)
        (setq positions (cons (minesweeper-point-position) positions))
      );while
      (setq n rayon)
      ;; moving up-right
      (while (>= (setq n (- n 1)) 0)
        (minesweeper-move-up)
        (minesweeper-move-right)
        (setq positions (cons (minesweeper-point-position) positions))
      );while
      (setq n rayon)
      positions
    );let*
  );save-excursion
);defun

(defun minesweeper-change-char nil
  "print the following char"
  (let* (
      (inhibit-read-only t)
      (char (char-after))
      (following-char
        (cond
          ((= char ?0) ?1)
          ((= char ?1) ?2)
          ((= char ?2) ?3)
          ((= char ?3) ?4)
          ((= char ?4) ?5)
          ((= char ?5) ?6)
          ((= char ?6) ?7)
          ((= char ?7) ?8)
          ((= char ?8) ?!)
          ((= char ?!) ?‽)
          ((= char ?‽) ?0)
          (t ?0)
        );cond
      )
    )
    (delete-forward-char 1)
    (insert following-char)
  );let
);defun

(defun minesweeper-print-positions (positions)
  "inserts colored char at every entered position"
  (save-excursion
    (let (
        (inhibit-read-only t)
      )
      (while (setq y (pop positions) x (pop y))
        (minesweeper-move-to x y)
        (minesweeper-change-char)
      );while
    );let
  );save-excursion
);defun

(defun minesweeper-explosion nil
  "Animation to end the game"
  (let ((n 0))
    (while t
      (sit-for 0.01)
      (minesweeper-print-positions (minesweeper-circle (setq n (+ n 1))))
    );while
  );let
);defun

;; =================================================================================
;; Minesweeper
;; =================================================================================

(defun minesweeper (&optional n m mines)
  "starts a minesweeper game
  optional arguments n m and mines define the size of the game and the number of mines in it"
  (interactive)
  (switch-to-buffer minesweeper-buffer-name)
  ;; setting revealed squares to zero
  (setq minesweeper-revealed-squares 0)
  ;; setting n, m, mines value if they are defined
  (when n (setq minesweeper-height n))
  (when m (setq minesweeper-width m))
  (when mines (setq minesweeper-mines mines))
  ;; changing mode
  (minesweeper-mode)
  ;; creating aned displaying the grid
  (minesweeper-init-matrix)
  (minesweeper-init-display minesweeper-height minesweeper-width)
  ;; placing the point
  (beginning-of-buffer)
);defun

(provide 'minesweeper)
