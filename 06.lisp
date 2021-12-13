(ql:quickload :cl-ppcre)

(defun read-input (file)
  (with-open-file (in file)
    (loop
      for line = (read-line in nil)
      while line
      collect (mapcar #'parse-integer
                      (cl-ppcre:all-matches-as-strings "(\\d+)" line)))))

(defun age (population)
  (let ((new-generation (make-hash-table)))
    (maphash (lambda (fish-state fish-count)
               (if (equal 0 fish-state)
                 (progn
                   (incf (gethash 6 new-generation 0) fish-count)  ;; current fish is now 6
                   (incf (gethash 8 new-generation 0) fish-count)) ;; new fish age 8
                 (incf (gethash (1- fish-state) new-generation 0) fish-count))) ;; next gen is all 1 year younger
             population)
    new-generation))

(defun build-initial-generaiton (input)
  (let ((input (car (read-input input)))
        (population (make-hash-table)))
    (loop for fish-state in input
          do (incf (gethash fish-state population 0)))
    population))

(defun simulate (days input)
  (let ((population (build-initial-generaiton input)))
    (loop for day from 1 to days
          do (setf population (age population))
          finally (return population))))

(defun simulate-sum-fish (days input)
  (loop for fish-state being the hash-key
          using (hash-value spawn-count) of (simulate days input)
        summing spawn-count))

(format t "Problem 06 A: ~a~%" (simulate-sum-fish 80 "06.input"))
(format t "Problem 06 B: ~a~%" (simulate-sum-fish 256 "06.input"))
