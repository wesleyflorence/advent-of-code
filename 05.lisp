(ql:quickload :cl-ppcre)

;;; Read in the file
(defun read-input (file)
  (with-open-file (in file)
    (loop
      for line = (read-line in nil)
      while line
      collect (mapcar #'parse-integer
                      (cl-ppcre:all-matches-as-strings "(\\d+)" line)))))

(defparameter *coordinates* (read-input "input/05.input"))

(defun line-orientation (coordinates diagonal)
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
                                        (fill-points y1 y2)))
                               (diagonal (fill-points-diagonal x1 y1 x2 y2))))))

(defun fill-points-diagonal (x1 y1 x2 y2)
  (let* ((x-fill (fill-points x1 x2))
         (y-fill (fill-points y1 y2))
         (x-final (if (> x1 x2) (reverse x-fill) x-fill))
         (y-final (if (> y1 y2) (reverse y-fill) y-fill)))
      (merge-diagonal x-final y-final)))

(defun merge-diagonal (list1 list2)
  (loop for x in list1
        for y in list2
        collect (list x y)))

(defun fill-points (point1 point2)
  (loop for it from (min point1 point2) to (max point1 point2)
        collect it))

(defun get-points (coordinates diagonal)
  (remove nil (line-orientation coordinates diagonal)))

(defun build-coordinate-counts (coordinates diagonal)
  (let ((table (make-hash-table :test 'equalp)))
    (mapc (lambda (coordinate)
            (if (gethash coordinate table)
                (setf (gethash coordinate table)
                      (+ (gethash coordinate table) 1))
                (setf (gethash coordinate table) 1)))
          (reduce #'append (get-points coordinates diagonal)))
    table))

(defun find-overlaps (coordinates &key (diagonal nil))
  (length (loop for key
    being the hash-key
    using (hash-value val) of (build-coordinate-counts coordinates diagonal)
      if (> val 1)
        collect key)))

(format t "Problem 05 A: ~a~%" (find-overlaps *coordinates*))
(format t "Problem 05 B: ~a~%" (find-overlaps *coordinates* :diagonal t))
