(defparameter *test* '(
                      ("forward" 5)
                      ("down" 5)
                      ("forward" 8)
                      ("up" 3)
                      ("down" 8)
                      ("forward" 2)))

(defparameter *directions* (mapcar #'(lambda (s)
                                       (let* ((pair (uiop:split-string s :separator " "))
                                              (dir (car pair))
                                              (paces (parse-integer (cadr pair))))
                                         (list dir paces)))
                                   (uiop:read-file-lines "02.input")))

;; Part A
(defun calculate-position (directions)
  (let ((forward 0)
        (depth 0))
    (loop for (dir paces) in directions
          do (cond
               ((string-equal dir "forward") (setf forward (+ forward paces)))
               ((string-equal dir "down") (setf depth (+ depth paces)))
               ((string-equal dir "up") (setf depth (- depth paces)))))
    (* forward depth)))

(calculate-position *test*)
(calculate-position *directions*) ; 1698735

;; Part B
(defun calculate-aim-position (directions)
  (let ((aim 0)
        (forward 0)
        (depth 0))
    (loop for (dir paces) in directions
          do (cond
               ((string-equal dir "down") (setf aim (+ aim paces)))
               ((string-equal dir "up") (setf aim (- aim paces)))
               ((string-equal dir "forward")
                (progn
                  (setf forward (+ forward paces))
                  (setf depth (+ depth (* paces aim)))))
               ))
    (* forward depth)))

(calculate-aim-position *test*)
(calculate-aim-position *directions*) ; 1594785890
