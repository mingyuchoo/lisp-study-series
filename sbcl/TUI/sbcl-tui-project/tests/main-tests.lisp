(defpackage sbcl-tui-project-tests
  (:use :cl :sbcl-tui-project)
  (:export :run-tests))

(in-package :sbcl-tui-project-tests)

;;; ============================================================
;;; Screen mock for side-effect function testing
;;; ============================================================

(defstruct mock-screen
  "A mock screen object that records drawing commands instead of writing to terminal."
  (width-val   80 :type fixnum)
  (height-val  24 :type fixnum)
  (attrs       '() :type list)
  (cursor-y    0   :type fixnum)
  (cursor-x    0   :type fixnum)
  (commands    '() :type list))

(defun mock-screen-push-command (mock cmd)
  "Push a command record onto the mock screen's command list."
  (push cmd (mock-screen-commands mock)))

;;; Implement screen protocol for mock-screen
(defmethod sbcl-tui-project::scr-width ((scr mock-screen))
  (mock-screen-width-val scr))

(defmethod sbcl-tui-project::scr-height ((scr mock-screen))
  (mock-screen-height-val scr))

(defmethod sbcl-tui-project::scr-move ((scr mock-screen) y x)
  (setf (mock-screen-cursor-y scr) y
        (mock-screen-cursor-x scr) x)
  (mock-screen-push-command scr (list :move y x)))

(defmethod sbcl-tui-project::scr-add-string ((scr mock-screen) str)
  (mock-screen-push-command scr (list :add-string str)))

(defmethod sbcl-tui-project::scr-set-attrs ((scr mock-screen) attrs)
  (setf (mock-screen-attrs scr) attrs)
  (mock-screen-push-command scr (list :set-attrs attrs)))

(defmethod sbcl-tui-project::scr-clear ((scr mock-screen))
  (setf (mock-screen-commands scr) nil)
  (mock-screen-push-command scr (list :clear)))

(defmethod sbcl-tui-project::scr-refresh ((scr mock-screen))
  (mock-screen-push-command scr (list :refresh)))

(defun mock-screen-find-command (mock command-type)
  "Find all commands of given type from mock screen."
  (remove-if-not (lambda (cmd) (eq (first cmd) command-type))
                 (mock-screen-commands mock)))

(defun mock-screen-find-string (mock substring)
  "Check if any add-string command contains the given substring."
  (some (lambda (cmd)
          (and (eq (first cmd) :add-string)
               (search substring (second cmd))))
        (mock-screen-commands mock)))

;;; ============================================================
;;; Pure function tests - ui.lisp
;;; ============================================================

(defun test-make-padded-string ()
  "Test that make-padded-string pads correctly."
  (let ((result (sbcl-tui-project::make-padded-string "Hi" 10)))
    (assert (= 10 (length result))
            () "Expected length 10, got ~A" (length result))
    (assert (string= "Hi" (subseq result 0 2))
            () "Expected 'Hi' prefix")))

(defun test-make-padded-string-overflow ()
  "Test that make-padded-string handles text longer than total-width."
  (let ((result (sbcl-tui-project::make-padded-string "Hello" 3)))
    (assert (string= "Hello" result)
            () "Expected 'Hello' unchanged when text exceeds width")))

(defun test-make-padded-string-exact ()
  "Test that make-padded-string handles exact-width text."
  (let ((result (sbcl-tui-project::make-padded-string "Hi" 2)))
    (assert (= 2 (length result))
            () "Expected length 2 for exact fit")
    (assert (string= "Hi" result)
            () "Expected 'Hi' with no padding")))

(defun test-make-padded-string-custom-char ()
  "Test that make-padded-string uses custom pad character."
  (let ((result (sbcl-tui-project::make-padded-string "A" 5 #\-)))
    (assert (string= "A----" result)
            () "Expected 'A----', got ~A" result)))

(defun test-center-offset ()
  "Test center offset calculation."
  (assert (= 5 (sbcl-tui-project::center-offset 10 20))
          () "Expected offset 5 for text=10, width=20")
  (assert (= 0 (sbcl-tui-project::center-offset 30 20))
          () "Expected offset 0 when text wider than width"))

(defun test-center-offset-odd ()
  "Test center offset with odd difference."
  (assert (= 4 (sbcl-tui-project::center-offset 11 20))
          () "Expected offset 4 for text=11, width=20 (floor)"))

(defun test-format-menu-item-selected ()
  "Test that selected menu item has > marker."
  (let ((result (sbcl-tui-project::format-menu-item "Hello" t)))
    (assert (search ">" result)
            () "Expected '>' in selected item: ~A" result)
    (assert (search "Hello" result)
            () "Expected 'Hello' in selected item: ~A" result)))

(defun test-format-menu-item-unselected ()
  "Test that unselected menu item has no > marker."
  (let ((result (sbcl-tui-project::format-menu-item "Hello" nil)))
    (assert (not (search ">" result))
            () "Expected no '>' in unselected item: ~A" result)
    (assert (search "Hello" result)
            () "Expected 'Hello' in unselected item: ~A" result)))

(defun test-make-box-top-border ()
  "Test box top border generation."
  (let ((result (sbcl-tui-project::make-box-top-border 6)))
    (assert (char= #\┌ (char result 0))
            () "Expected top-left corner")
    (assert (char= #\┐ (char result (1- (length result))))
            () "Expected top-right corner")
    (assert (= 4 (count #\─ result))
            () "Expected 4 horizontal chars for width 6")))

(defun test-make-box-bottom-border ()
  "Test box bottom border generation."
  (let ((result (sbcl-tui-project::make-box-bottom-border 6)))
    (assert (char= #\└ (char result 0))
            () "Expected bottom-left corner")
    (assert (char= #\┘ (char result (1- (length result))))
            () "Expected bottom-right corner")
    (assert (= 4 (count #\─ result))
            () "Expected 4 horizontal chars for width 6")))

;;; ============================================================
;;; Pure function tests - main.lisp
;;; ============================================================

(defun test-app-state-creation ()
  "Test app-state struct default values."
  (let ((state (sbcl-tui-project::make-app-state)))
    (assert (= 0 (sbcl-tui-project::app-state-selected-index state))
            () "Expected selected-index 0")
    (assert (eq t (sbcl-tui-project::app-state-running-p state))
            () "Expected running-p t")
    (assert (string= "" (sbcl-tui-project::app-state-message state))
            () "Expected empty message")))

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

(defun test-move-selection-down-wrap ()
  "Test that move-selection-down wraps from last to first."
  (let ((state (sbcl-tui-project::make-app-state))
        (len (length sbcl-tui-project::*menu-items*)))
    ;; Move to last item
    (dotimes (i (1- len))
      (sbcl-tui-project::move-selection-down state))
    (assert (= (1- len) (sbcl-tui-project::app-state-selected-index state))
            () "Expected to be at last item")
    ;; One more down should wrap to 0
    (sbcl-tui-project::move-selection-down state)
    (assert (= 0 (sbcl-tui-project::app-state-selected-index state))
            () "Expected wrap to first item")))

(defun test-handle-menu-action-hello ()
  "Test menu action index 0: Hello World."
  (let ((state (sbcl-tui-project::make-app-state)))
    (sbcl-tui-project::handle-menu-action state 0)
    (assert (search "Hello" (sbcl-tui-project::app-state-message state))
            () "Expected Hello in message")
    (assert (eq t (sbcl-tui-project::app-state-running-p state))
            () "Expected still running")))

(defun test-handle-menu-action-sysinfo ()
  "Test menu action index 1: System Info."
  (let ((state (sbcl-tui-project::make-app-state)))
    (sbcl-tui-project::handle-menu-action state 1)
    (let ((msg (sbcl-tui-project::app-state-message state)))
      (assert (search "SBCL" msg)
              () "Expected 'SBCL' in system info: ~A" msg)
      (assert (plusp (length msg))
              () "Expected non-empty system info"))))

(defun test-handle-menu-action-about ()
  "Test menu action index 2: About."
  (let ((state (sbcl-tui-project::make-app-state)))
    (sbcl-tui-project::handle-menu-action state 2)
    (assert (search "croatoan" (sbcl-tui-project::app-state-message state))
            () "Expected 'croatoan' in about message")))

(defun test-handle-menu-action-quit ()
  "Test menu action index 3: Quit."
  (let ((state (sbcl-tui-project::make-app-state)))
    (sbcl-tui-project::handle-menu-action state 3)
    (assert (null (sbcl-tui-project::app-state-running-p state))
            () "Expected running-p nil after quit")))

(defun test-handle-menu-action-default ()
  "Test menu action with unknown index: no crash, state unchanged."
  (let ((state (sbcl-tui-project::make-app-state)))
    (sbcl-tui-project::handle-menu-action state 99)
    (assert (string= "" (sbcl-tui-project::app-state-message state))
            () "Expected message unchanged for unknown index")
    (assert (eq t (sbcl-tui-project::app-state-running-p state))
            () "Expected still running for unknown index")))

;;; ============================================================
;;; Side-effect function tests (using mock screen)
;;; ============================================================

(defun test-draw-title-bar ()
  "Test that draw-title-bar writes title centered with reverse attribute."
  (let ((mock (make-mock-screen :width-val 40 :height-val 10)))
    (sbcl-tui-project::draw-title-bar mock "Test Title")
    (assert (mock-screen-find-string mock "Test Title")
            () "Expected 'Test Title' drawn on screen")
    ;; Should have set :reverse attribute
    (assert (some (lambda (cmd)
                    (and (eq (first cmd) :set-attrs)
                         (member :reverse (second cmd))))
                  (mock-screen-commands mock))
            () "Expected :reverse attribute set")
    ;; Should have reset attributes at the end
    (let ((last-attr (first (mock-screen-find-command mock :set-attrs))))
      (assert (null (second last-attr))
              () "Expected attributes reset to nil at end"))))

(defun test-draw-status-bar ()
  "Test that draw-status-bar writes message at bottom with reverse attribute."
  (let ((mock (make-mock-screen :width-val 40 :height-val 10)))
    (sbcl-tui-project::draw-status-bar mock "Status Msg")
    (assert (mock-screen-find-string mock "Status Msg")
            () "Expected 'Status Msg' drawn on screen")
    ;; Should draw at bottom row (height - 1 = 9)
    (assert (some (lambda (cmd)
                    (and (eq (first cmd) :move)
                         (= 9 (second cmd))))
                  (mock-screen-commands mock))
            () "Expected move to row 9 (bottom)")))

(defun test-draw-menu-items ()
  "Test that draw-menu-items renders all items with correct formatting."
  (let ((mock (make-mock-screen :width-val 40 :height-val 20))
        (items '("Alpha" "Beta" "Gamma")))
    (sbcl-tui-project::draw-menu-items mock items 1 :start-y 3 :start-x 4)
    ;; All items should appear
    (assert (mock-screen-find-string mock "Alpha")
            () "Expected 'Alpha' drawn")
    (assert (mock-screen-find-string mock "Beta")
            () "Expected 'Beta' drawn")
    (assert (mock-screen-find-string mock "Gamma")
            () "Expected 'Gamma' drawn")
    ;; Selected item (index 1, "Beta") should have > marker
    (assert (some (lambda (cmd)
                    (and (eq (first cmd) :add-string)
                         (search ">" (second cmd))
                         (search "Beta" (second cmd))))
                  (mock-screen-commands mock))
            () "Expected '>' marker on selected item Beta")
    ;; Unselected items should NOT have > marker
    (assert (some (lambda (cmd)
                    (and (eq (first cmd) :add-string)
                         (search "Alpha" (second cmd))
                         (not (search ">" (second cmd)))))
                  (mock-screen-commands mock))
            () "Expected no '>' marker on unselected item Alpha")))

(defun test-draw-box-area ()
  "Test that draw-box-area draws corners and borders."
  (let ((mock (make-mock-screen :width-val 40 :height-val 20)))
    (sbcl-tui-project::draw-box-area mock 2 2 10 5)
    ;; Top border with corners
    (assert (mock-screen-find-string mock "┌")
            () "Expected top-left corner")
    (assert (mock-screen-find-string mock "┐")
            () "Expected top-right corner")
    ;; Bottom border with corners
    (assert (mock-screen-find-string mock "└")
            () "Expected bottom-left corner")
    (assert (mock-screen-find-string mock "┘")
            () "Expected bottom-right corner")
    ;; Side borders
    (assert (mock-screen-find-string mock "│")
            () "Expected side border")))

(defun test-render ()
  "Test that render draws title, menu, and status bar."
  (let ((mock (make-mock-screen :width-val 40 :height-val 20))
        (state (sbcl-tui-project::make-app-state)))
    (sbcl-tui-project::render mock state)
    ;; Should have cleared screen
    (assert (some (lambda (cmd) (eq (first cmd) :clear))
                  (mock-screen-commands mock))
            () "Expected screen cleared")
    ;; Should have refreshed screen
    (assert (some (lambda (cmd) (eq (first cmd) :refresh))
                  (mock-screen-commands mock))
            () "Expected screen refreshed")
    ;; Should draw title
    (assert (mock-screen-find-string mock sbcl-tui-project::*app-title*)
            () "Expected app title drawn")
    ;; Should draw status bar hints
    (assert (mock-screen-find-string mock "Quit")
            () "Expected 'Quit' in status bar")))

(defun test-render-with-message ()
  "Test that render shows message when state has one."
  (let ((mock (make-mock-screen :width-val 40 :height-val 20))
        (state (sbcl-tui-project::make-app-state)))
    (setf (sbcl-tui-project::app-state-message state) "Test Message")
    (sbcl-tui-project::render mock state)
    (assert (mock-screen-find-string mock "Test Message")
            () "Expected 'Test Message' drawn on screen")))

;;; ============================================================
;;; Test runner
;;; ============================================================

(defun run-tests ()
  (format t "~%Running tests...~%")
  (let ((tests '(;; Pure function tests - ui.lisp
                 test-make-padded-string
                 test-make-padded-string-overflow
                 test-make-padded-string-exact
                 test-make-padded-string-custom-char
                 test-center-offset
                 test-center-offset-odd
                 test-format-menu-item-selected
                 test-format-menu-item-unselected
                 test-make-box-top-border
                 test-make-box-bottom-border
                 ;; Pure function tests - main.lisp
                 test-app-state-creation
                 test-move-selection
                 test-move-selection-down-wrap
                 test-handle-menu-action-hello
                 test-handle-menu-action-sysinfo
                 test-handle-menu-action-about
                 test-handle-menu-action-quit
                 test-handle-menu-action-default
                 ;; Side-effect function tests (mock screen)
                 test-draw-title-bar
                 test-draw-status-bar
                 test-draw-menu-items
                 test-draw-box-area
                 test-render
                 test-render-with-message))
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
