(in-package :advent)

(defun read-input ()
  (mapcar (lambda (s)
            (map 'list #'digit-char-p s))
          (uiop:read-file-lines "input/09.test")))

(defparameter *floor-nodes* nil)
(defparameter *neighbors* nil)

(defun build-neighbors (x y width height)
  (loop for neighbor-y from (max (- y 1) 0) to (min (+ y 1) height) do
    (loop for neighbor-x from (max (- x 1) 0) to (min (+ x 1) width) do
      (if (not (and (equal x neighbor-x)
                    (equal y neighbor-y)))
          (setf (gethash (format nil "[~a, ~a]" x y) *neighbors* '())
                (concatenate 'list (gethash (format nil "[~a, ~a]" x y) *neighbors* '())
                             (list (format nil "[~a, ~a]" neighbor-x neighbor-y))))))))

(defun build-cave-graph (input)
  (setf *floor-nodes* (make-hash-table :test 'equal))
  (setf *neighbors* (make-hash-table :test 'equal))
  (let ((height (length input))
        (width (length (car input))))
    (loop for y-row in input
          for y from 0 do
            (loop for node in y-row
                  for x from 0 do
                    (setf (gethash
                           (format nil "[~a, ~a]" x y)
                           *floor-nodes*)
                          node)
                    (build-neighbors x y width height)))))

(defun d9/summary ()
  (format t "Problem 09 A: ~a~%" "yo")
  (format t "Problem 09 B: ~a~%" "yo"))
