;; Load file
(defparameter *directions*
  (mapcar #'(lambda (s)
              (let* ((pair (uiop:split-string s :separator " "))
                     (dir (car pair))
                     (paces (parse-integer (cadr pair))))
                (list dir paces)))
          (uiop:read-file-lines "input/02.input")))

;; Part A
(defun calculate-position (directions)
  (let ((forward 0)
        (depth 0))
    (loop for (dir paces) in directions
          do (cond
               ((string-equal dir "forward") (incf forward paces))
               ((string-equal dir "down") (incf depth paces))
               ((string-equal dir "up") (decf depth paces))))
    (* forward depth)))

;; Part B
(defun calculate-aim-position (directions)
  (let ((aim 0)
        (forward 0)
        (depth 0))
    (loop for (dir paces) in directions
          do (cond
               ((string-equal dir "down") (incf aim paces))
               ((string-equal dir "up") (decf aim paces))
               ((string-equal dir "forward")
                (progn
                  (incf forward paces)
                  (incf depth (* paces aim))))
               ))
    (* forward depth)))

;; Scenarios
(format t "Problem 02 A: ~a~%" (calculate-position *directions*)) ; 1698735
(format t "Problem 02 B: ~a~%" (calculate-aim-position *directions*)) ; 1594785890
