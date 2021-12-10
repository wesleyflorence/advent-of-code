(ql:quickload :cl-ppcre)

;;; Read in the file
(defun read-input (file)
  (with-open-file (in file)
    (loop
      for line = (read-line in nil)
      while line
      collect (mapcar #'parse-integer
                      (cl-ppcre:all-matches-as-strings "(\\d+)" line)))))

(defparameter *coordinates* (read-input "05.input"))
; Find max value, currently everything is < 1000
(defparameter *max-value* (reduce #'max (loop for l in *coordinates*
      collect (max (reduce #'max l)))))

(defun line-orientation (coordinates)
  (loop for coordinate in coordinates
        append (loop for (x1 y1 x2 y2) in (list coordinate)
                     collect (cond
                               ((equalp y1 y2)
                                (mapcar (lambda (point)
                                          (list point y1))
                                        (fill-points x1 x2)))
                               ((equalp x1 x2)
                                (mapcar (lambda (point)
                                          (list x1 point))
                                        (fill-points y1 y2)))))))


(defun fill-points (point1 point2)
  (loop for it from (min point1 point2) to (max point1 point2)
        collect it))

(defun get-points (coordinates)
  (remove nil (line-orientation coordinates)))

(defun build-coordinate-counts (coordinates)
  (let ((table (make-hash-table :test 'equalp)))
    (mapc (lambda (coordinate)
            (if (gethash coordinate table)
                (setf (gethash coordinate table)
                      (+ (gethash coordinate table) 1))
                (setf (gethash coordinate table) 1)))
          (reduce #'append (get-points coordinates)))
    table))

(defvar *table* (build-coordinate-counts *coordinates*))

(length (loop for key
    being the hash-key
    using (hash-value val) of (build-coordinate-counts *coordinates*)
      if (> val 1)
        collect key))
