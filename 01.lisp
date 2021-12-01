;;; Read in the file
(defparameter *depths* (mapcar #'parse-integer (uiop:read-file-lines "01.input")))

;;; Part 1
(defun process-depths (depths-list)
  (loop for (first second) on depths-list
        count (if second (> second first))))

(defun find-depth-count ()
  (process-depths *depths*))

;;; Part 2
(defun process-window (depths-list)
  (loop for (first second third fourth) on depths-list
        count (if fourth
                  (> (+ second third fourth) (+ first second third)))))

(defun find-windowed-depth-count ()
  (process-window *depths*))

(defun solve-day-1 ()
  (format t "Problem 01 A: ~a~%" (find-depth-count))
  (format t "Problem 01 B: ~a~%" (find-windowed-depth-count)))
