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

;;; ============================================================
;;; Side-effect functions - Screen drawing
;;; ============================================================

(defun draw-title-bar (scr title)
  "Draw a reversed-color title bar at the top of the screen."
  (let ((w (width scr)))
    (setf (attributes scr) '(:reverse))
    (move scr 0 0)
    (add-string scr (make-padded-string "" w))
    (move scr 0 (center-offset (length title) w))
    (add-string scr title)
    (setf (attributes scr) '())))

(defun draw-status-bar (scr message)
  "Draw a reversed-color status bar at the bottom of the screen."
  (let ((w (width scr))
        (h (height scr)))
    (setf (attributes scr) '(:reverse))
    (move scr (1- h) 0)
    (add-string scr (make-padded-string "" w))
    (move scr (1- h) 1)
    (add-string scr message)
    (setf (attributes scr) '())))

(defun draw-menu-items (scr items selected-index &key (start-y 3) (start-x 4))
  "Draw a list of menu items, highlighting the selected one."
  (loop for item in items
        for i from 0
        for y = (+ start-y i)
        do (move scr y start-x)
           (if (= i selected-index)
               (progn
                 (setf (attributes scr) '(:bold :reverse))
                 (add-string scr (format nil " > ~A " item))
                 (setf (attributes scr) '()))
               (add-string scr (format nil "   ~A  " item)))))

(defun draw-box-area (scr y x w h)
  "Draw a Unicode box at position (Y, X) with width W and height H."
  (let ((horizontal (make-string (- w 2) :initial-element #\─)))
    ;; Top border
    (move scr y x)
    (add-string scr (concatenate 'string "┌" horizontal "┐"))
    ;; Side borders
    (loop for row from 1 below (1- h)
          do (move scr (+ y row) x)
             (add-string scr "│")
             (move scr (+ y row) (+ x w -1))
             (add-string scr "│"))
    ;; Bottom border
    (move scr (+ y h -1) x)
    (add-string scr (concatenate 'string "└" horizontal "┘"))))

