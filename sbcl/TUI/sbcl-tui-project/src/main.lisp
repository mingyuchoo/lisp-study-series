(in-package :sbcl-tui-project)

;;; ============================================================
;;; Application state (pure data)
;;; ============================================================

(defstruct app-state
  "Holds the mutable state of the TUI application."
  (selected-index 0  :type fixnum)
  (running-p      t  :type boolean)
  (message        "" :type string))

(defparameter *menu-items*
  '("Hello World"
    "Show System Info"
    "About"
    "Quit")
  "List of menu items displayed in the TUI.")

(defparameter *app-title* "sbcl-tui-project - TUI Application")

;;; ============================================================
;;; Menu action handlers (side-effect boundary)
;;; ============================================================

(defun handle-menu-action (state index)
  "Process the selected menu item and update state."
  (case index
    (0 (setf (app-state-message state)
             "Hello, World! Welcome to the TUI."))
    (1 (setf (app-state-message state)
             (format nil "SBCL ~A | ~A ~A"
                     (lisp-implementation-version)
                     (software-type)
                     (machine-type))))
    (2 (setf (app-state-message state)
             "Built with Common Lisp & croatoan."))
    (3 (setf (app-state-running-p state) nil)))
  state)

;;; ============================================================
;;; Render (side-effect: screen output)
;;; ============================================================

(defun render (scr state)
  "Render the full TUI screen from the current state."
  (scr-clear scr)
  (draw-title-bar scr *app-title*)

  ;; Menu box
  (let* ((menu-y 2)
         (menu-x 2)
         (menu-w 30)
         (menu-h (+ (length *menu-items*) 2)))
    (draw-box-area scr menu-y menu-x menu-w menu-h)
    (draw-menu-items scr *menu-items*
                     (app-state-selected-index state)
                     :start-y (1+ menu-y)
                     :start-x (+ menu-x 2)))

  ;; Message area
  (let ((msg (app-state-message state)))
    (when (plusp (length msg))
      (scr-move scr (+ 4 (length *menu-items*)) 4)
      (scr-set-attrs scr '(:bold))
      (scr-add-string scr msg)
      (scr-set-attrs scr '())))

  ;; Status bar
  (draw-status-bar scr "[Up/k]Up [Down/j]Down [Enter]Select [q]Quit")
  (scr-refresh scr))

;;; ============================================================
;;; Event handling (pure logic for key->state transitions)
;;; ============================================================

(defun move-selection-up (state)
  "Move selection up, wrapping around."
  (let ((idx (app-state-selected-index state))
        (len (length *menu-items*)))
    (setf (app-state-selected-index state)
          (mod (1- idx) len)))
  state)

(defun move-selection-down (state)
  "Move selection down, wrapping around."
  (let ((idx (app-state-selected-index state))
        (len (length *menu-items*)))
    (setf (app-state-selected-index state)
          (mod (1+ idx) len)))
  state)

;;; ============================================================
;;; Main entry point
;;; ============================================================

(defun main ()
  "Entry point for the TUI application."
  (with-screen (scr :input-echoing nil
                    :input-blocking t
                    :cursor-visible nil
                    :enable-colors t)
    (let ((state (make-app-state)))
      (render scr state)
      (event-case (scr event)
        (#\q
         (return-from event-case))
        (:up
         (move-selection-up state)
         (render scr state))
        (:down
         (move-selection-down state)
         (render scr state))
        (#\k
         (move-selection-up state)
         (render scr state))
        (#\j
         (move-selection-down state)
         (render scr state))
        (#\Newline
         (handle-menu-action state (app-state-selected-index state))
         (if (app-state-running-p state)
             (render scr state)
             (return-from event-case)))
        (otherwise nil)))))

