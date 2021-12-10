(ql:quickload :cl-ppcre)
(defun read-input (file)
  (with-open-file (in file)
    (loop
      for line = (read-line in nil)
      while line
      collect (mapcar #'parse-integer
                      (cl-ppcre:all-matches-as-strings "(\\d+)" line)))))

(defparameter *population* (car (read-input "06.input")))

(defun spawn (population)
  (loop for fish in population
        if (equal fish 0) collect 8))

(defun age (population)
  (mapcar (lambda (fish)
            (if (equal fish 0) 6 (- fish 1)))
          population))

(defun tick (population)
  (append (age population)
          (spawn population)))

(defun simulate (cycles population)
  (loop for i from 1 to cycles
        do (setf population (tick population))
        finally (return population)))

(length (simulate 80 *population*))
