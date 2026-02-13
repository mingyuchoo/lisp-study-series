(in-package :sbcl-tui-project)

;;; ============================================================
;;; Screen protocol - generic functions for testability
;;; Default methods delegate to croatoan. Tests can specialize
;;; these for mock screen objects without requiring a terminal.
;;; ============================================================

(defgeneric scr-width (scr)
  (:documentation "Return the width of the screen.")
  (:method (scr) (width scr)))

(defgeneric scr-height (scr)
  (:documentation "Return the height of the screen.")
  (:method (scr) (height scr)))

(defgeneric scr-move (scr y x)
  (:documentation "Move the cursor to position (Y, X).")
  (:method (scr y x) (move scr y x)))

(defgeneric scr-add-string (scr str)
  (:documentation "Write STR at the current cursor position.")
  (:method (scr str) (add-string scr str)))

(defgeneric scr-set-attrs (scr attrs)
  (:documentation "Set screen attributes (e.g. :reverse, :bold).")
  (:method (scr attrs) (setf (attributes scr) attrs)))

(defgeneric scr-clear (scr)
  (:documentation "Clear the screen.")
  (:method (scr) (clear scr)))

(defgeneric scr-refresh (scr)
  (:documentation "Refresh the screen to display pending changes.")
  (:method (scr) (refresh scr)))
