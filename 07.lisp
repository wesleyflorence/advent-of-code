(ql:quickload :cl-ppcre)

(defun read-input (file)
  (with-open-file (in file)
    (loop
      for line = (read-line in nil)
      while line
      return (mapcar #'parse-integer
                      (cl-ppcre:all-matches-as-strings "(\\d+)" line)))))

(defun get-step-list (pos steps &optional fuel-doubles)
  (let* ((step-list
           (mapcar (lambda (crab-pos)
                     (abs (- pos crab-pos)))
                   steps))
         (final-list
           (if fuel-doubles
               (mapcar (lambda (step)
                         (loop for i from 0 to step summing i))
                       step-list)
               step-list)))
    (reduce #'+ final-list)))

(defun find-min-steps (input &optional fuel-doubles)
  (let ((steps (read-input input)))
    (reduce #'min (loop for pos from 1 to (reduce #'max steps)
                        collect (get-step-list pos steps fuel-doubles)))))

(format t "Problem 07 A: ~a~%" (find-min-steps "07.input"))
(format t "Problem 07 B: ~a~%" (find-min-steps "07.input" t))