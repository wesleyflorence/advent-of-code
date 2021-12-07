;;; Read in the file
(defvar *diagnostic*
  (mapcar #'(lambda (s) (map 'list #'digit-char-p s))
          (uiop:read-file-lines "03.input")))

;; Part A
(defun build-digit-count-list (diagnostic)
  (reduce (lambda (a b) (map 'list #'+ a b)) diagnostic))

(defun find-gamma-binary (diagnostic)
  (let* ((ln (/ (length diagnostic) 2))
         (ones (build-digit-count-list diagnostic)))
    (mapcar (lambda (digit)
              (cond
                ((> digit ln) 1)
                (t 0)))
            ones)))

(defun gamma (diagnostic)
  (parse-integer (map 'string #'digit-char
                      (find-gamma-binary diagnostic)) :radix 2))

(defun epsilon (diagnostic)
  (parse-integer (map 'string #'(lambda (c)
                                  (digit-char (* (- c 1) -1)))
                      (find-gamma-binary diagnostic)) :radix 2))

(defun run-diagnostic (diagnostic)
  (* (gamma diagnostic) (epsilon diagnostic)))

;;; Part B
(defvar *match* nil)
(defvar *final* '())

(defun car-digit-count (diagnostic)
  (reduce (lambda (a b) (+ a (car b))) diagnostic :initial-value 0))

(defun get-1-or-0 (diagnostic &key (oxygen t))
  (let* ((ln (/ (length diagnostic) 2))
         (digit (car-digit-count diagnostic))
         (val (cond
                ((eql digit ln) (if oxygen 1 0))
                ((> digit ln) (if oxygen 1 0))
                (t (if oxygen 0 1)))))
    (setf *match* val)
    val))

(defun filter-diagnostics (diagnostic &key (oxygen t))
  (setf *match* nil)
  (setf diagnostic (map 'list #'rest
                        (remove-if-not
                         (lambda (bin)
                           (eql (get-1-or-0 diagnostic :oxygen oxygen)
                                (car bin)))
                         diagnostic)))
  (setf *final* (append *final* (list *match*)))
  diagnostic)

(defun build-final-list (diagnostic oxygen)
  (setf *final* nil)
  (setf *match* nil)
  (loop for i from 1 to (length (car diagnostic))
        when (eql 1 (length diagnostic))
          do (return (append *final* (car diagnostic)))
        do (setf diagnostic (filter-diagnostics diagnostic :oxygen oxygen))
        finally (return *final*)))

(defun filter-oxygen (diagnostic)
  (build-final-list diagnostic t))

(defun filter-carbon (diagnostic)
  (build-final-list diagnostic nil))

(defun run-life-support (diagnostic)
  (let ((oxygen (filter-oxygen diagnostic))
        (carbon (filter-carbon diagnostic)))
    (* (parse-integer (format nil "~{~A~}" (mapcar #'write-to-string oxygen)) :radix 2)
       (parse-integer (format nil "~{~A~}" (mapcar #'write-to-string carbon)) :radix 2))))

;;; Solutions
(format t "Problem 03 A: ~a~%" (run-diagnostic *diagnostic*))
(format t "Problem 03 B: ~a~%" (run-life-support *diagnostic*))
