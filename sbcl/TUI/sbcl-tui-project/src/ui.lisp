(in-package :sbcl-tui-project)

;;; ============================================================
;;; Pure functions - UI rendering helpers
;;; ============================================================

(defun make-padded-string (text total-width &optional (pad-char #\Space))
  "Create a string of TOTAL-WIDTH padded with PAD-CHAR after TEXT."
  (let* ((len (length text))
         (padding (max 0 (- total-width len))))
    (concatenate 'string text (make-string padding :initial-element pad-char))))

(defun center-offset (text-length total-width)
  "Calculate x offset to center text of TEXT-LENGTH in TOTAL-WIDTH."
  (max 0 (floor (- total-width text-length) 2)))

(defun format-menu-item (item selected-p)
  "Format a single menu item string. Returns highlighted or plain text."
  (if selected-p
      (format nil " > ~A " item)
      (format nil "   ~A  " item)))

(defun make-box-top-border (width)
  "Generate the top border string for a box of given WIDTH."
  (let ((horizontal (make-string (- width 2) :initial-element #\─)))
    (concatenate 'string "┌" horizontal "┐")))

(defun make-box-bottom-border (width)
  "Generate the bottom border string for a box of given WIDTH."
  (let ((horizontal (make-string (- width 2) :initial-element #\─)))
    (concatenate 'string "└" horizontal "┘")))

;;; ============================================================
;;; Side-effect functions - Screen drawing
;;; ============================================================

(defun draw-title-bar (scr title)
  "Draw a reversed-color title bar at the top of the screen."
  (let ((w (scr-width scr)))
    (scr-set-attrs scr '(:reverse))
    (scr-move scr 0 0)
    (scr-add-string scr (make-padded-string "" w))
    (scr-move scr 0 (center-offset (length title) w))
    (scr-add-string scr title)
    (scr-set-attrs scr '())))

(defun draw-status-bar (scr message)
  "Draw a reversed-color status bar at the bottom of the screen."
  (let ((w (scr-width scr))
        (h (scr-height scr)))
    (scr-set-attrs scr '(:reverse))
    (scr-move scr (1- h) 0)
    (scr-add-string scr (make-padded-string "" w))
    (scr-move scr (1- h) 1)
    (scr-add-string scr message)
    (scr-set-attrs scr '())))

(defun draw-menu-items (scr items selected-index &key (start-y 3) (start-x 4))
  "Draw a list of menu items, highlighting the selected one."
  (loop for item in items
        for i from 0
        for y = (+ start-y i)
        do (scr-move scr y start-x)
           (if (= i selected-index)
               (progn
                 (scr-set-attrs scr '(:bold :reverse))
                 (scr-add-string scr (format-menu-item item t))
                 (scr-set-attrs scr '()))
               (scr-add-string scr (format-menu-item item nil)))))

(defun draw-box-area (scr y x w h)
  "Draw a Unicode box at position (Y, X) with width W and height H."
  ;; Top border
  (scr-move scr y x)
  (scr-add-string scr (make-box-top-border w))
  ;; Side borders
  (loop for row from 1 below (1- h)
        do (scr-move scr (+ y row) x)
           (scr-add-string scr "│")
           (scr-move scr (+ y row) (+ x w -1))
           (scr-add-string scr "│"))
  ;; Bottom border
  (scr-move scr (+ y h -1) x)
  (scr-add-string scr (make-box-bottom-border w)))

