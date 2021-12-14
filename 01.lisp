;;; Read in the file
(defparameter *depths* (mapcar #'parse-integer (uiop:read-file-lines "input/01.input")))

(defmacro loop-depths ((&rest window) &body body)
  "Loops through the depth list, destructuring into a window, stops when last item is nil"
  `(loop for ,window on *depths*
         count (if (car (last (list ,@window))) ,@body)))

(defun find-paired-depth-count ()
  "Iterates over depths and counts increases between each depth pair"
  (loop-depths (depth-1 depth-2)
    (> depth-2 depth-1)))

(defun find-windowed-depth-count ()
  "Iterates over depths and counts increase between the sum of a window"
  (loop-depths (d1 d2 d3 d4)
    (> (+ d2 d3 d4) (+ d1 d2 d3))))

;; Solve Day 1
(format t "Problem 01 A: ~a~%" (find-paired-depth-count))
(format t "Problem 01 B: ~a~%" (find-windowed-depth-count))
