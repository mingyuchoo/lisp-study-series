(defpackage sbcl-tui-project-tests
  (:use :cl :sbcl-tui-project)
  (:export :run-tests))

(in-package :sbcl-tui-project-tests)

;;; ============================================================
;;; Pure function tests
;;; ============================================================

(defun test-make-padded-string ()
  "Test that make-padded-string pads correctly."
  (let ((result (sbcl-tui-project::make-padded-string "Hi" 10)))
    (assert (= 10 (length result))
            () "Expected length 10, got ~A" (length result))
    (assert (string= "Hi" (subseq result 0 2))
            () "Expected 'Hi' prefix")))

(defun test-center-offset ()
  "Test center offset calculation."
  (assert (= 5 (sbcl-tui-project::center-offset 10 20))
          () "Expected offset 5 for text=10, width=20")
  (assert (= 0 (sbcl-tui-project::center-offset 30 20))
          () "Expected offset 0 when text wider than width"))

(defun test-move-selection ()
  "Test selection movement with wrapping."
  (let ((state (sbcl-tui-project::make-app-state)))
    ;; Start at 0, move down
    (sbcl-tui-project::move-selection-down state)
    (assert (= 1 (sbcl-tui-project::app-state-selected-index state))
            () "Expected index 1 after move-down")
    ;; Move back up
    (sbcl-tui-project::move-selection-up state)
    (assert (= 0 (sbcl-tui-project::app-state-selected-index state))
            () "Expected index 0 after move-up")
    ;; Move up from 0 should wrap
    (sbcl-tui-project::move-selection-up state)
    (assert (= (1- (length sbcl-tui-project::*menu-items*))
               (sbcl-tui-project::app-state-selected-index state))
            () "Expected wrap to last item")))

;;; ============================================================
;;; Test runner
;;; ============================================================

(defun run-tests ()
  (format t "~%Running tests...~%")
  (let ((tests '(test-make-padded-string
                 test-center-offset
                 test-move-selection))
        (passed 0)
        (failed 0))
    (dolist (test tests)
      (handler-case
          (progn
            (funcall test)
            (format t "  PASS ~A~%" test)
            (incf passed))
        (error (e)
          (format t "  FAIL ~A: ~A~%" test e)
          (incf failed))))
    (format t "~%Results: ~A passed, ~A failed~%" passed failed)
    (when (plusp failed)
      (sb-ext:exit :code 1))))

