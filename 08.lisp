(in-package :advent)

(defun read-input ()
  (mapcar (lambda (s)
            (destructuring-bind (signals codes)
                (cl-ppcre:split "( \\| )" s :omit-unmatched-p t)
              (list (cl-ppcre:split "\\s+" signals)
                    (cl-ppcre:split "\\s+" codes))))
          (uiop:read-file-lines "input/08.test")))

;; Part A
(defun get-known-codes ()
  (mapcar (lambda (input)
            (let ((codes (second input)))
              (remove-if-not (lambda (code)
                           (cond ((equal (length code) 2) t)   ; 1
                                 ((equal (length code) 3) t)   ; 7
                                 ((equal (length code) 4) t)   ; 4
                                 ((equal (length code) 7) t))) ; 8
                         codes)))
          (read-input)))

(defun get-total-known-codes ()
  (reduce (lambda (a b)
            (+ a (length b)))
          (get-known-codes) :initial-value 0))

;; Part 2
;; len 2 -> 1
;; len 3 -> 7
;; len 4 -> 4
;; len 7 -> 8
;; len 5, contains both 1 chars -> 3
;; len 5, intersection with 4 is 2 -> 2
;; len 5, intersection with 4 is 3 -> 5
;; len 6, set-difference 4 is 1 -> 0
;; len 6, if both 1s are in -> 9
;; len 6, if only one 1 are in -> 6

(defun build-base-code-lookup (signals)
  (let ((table (make-hash-table)))
    (mapcar (lambda (signal)
              (cond ((equal (length signal) 2)
                     (setf (gethash 1 table) signal))
                    ((equal (length signal) 3)
                     (setf (gethash 7 table) signal))
                    ((equal (length signal) 4)
                     (setf (gethash 4 table) signal))
                    ((equal (length signal) 7)
                     (setf (gethash 8 table) signal))
                    ))
            signals)
    table))

(defun ascii-sum (str)
  (reduce (lambda (a b) (+ a (char-code b))) str :initial-value 0))

(defun flip-table (table)
  (let ((flipped (make-hash-table)))
    (loop for k being each hash-key of table
            using (hash-value v)
          do (setf (gethash (ascii-sum v) flipped) k))
    flipped))

(defun build-code-lookup (signals)
  (let ((table (build-base-code-lookup signals)))
    (mapcar (lambda (signal)
              (cond ((equal (length signal) 5)
                     (cond ((equal 3 (length (set-difference (coerce signal 'list)
                                                             (coerce (gethash 1 table) 'list))))
                            (setf (gethash 3 table) signal))
                           ((equal 2 (length (set-difference (coerce signal 'list)
                                                             (coerce (gethash 4 table) 'list))))
                            (setf (gethash 2 table) signal))
                           (t
                            (setf (gethash 5 table) signal))))
                    ((equal (length signal) 6)
                     (cond ((equal 3 (length (set-exclusive-or (coerce signal 'list)
                                                               (coerce (gethash 4 table) 'list))))
                            (setf (gethash 0 table) signal))
                           ((equal 4 (length (set-exclusive-or (coerce signal 'list)
                                                               (coerce (gethash 4 table) 'list))))
                            (setf (gethash 6 table) signal))
                           (t
                            (setf (gethash 9 table) signal))))))
              signals)
            (flip-table table)))

(defun decode (signals code)
  (let ((lookup (build-code-lookup signals)))
    (parse-integer (reduce (lambda (a b)
              (concatenate 'string a (write-to-string (gethash (ascii-sum b) lookup))))
            code :initial-value ""))))

(defun solve (input)
  (reduce (lambda (a b)
            (+ a (decode (first b) (second b))))
          input :initial-value 0))

(defun d8/summary ()
  (format t "Problem 08 A: ~a~%" (get-total-known-codes))
  (format t "Problem 08 B: ~a~%" (solve (read-input))))
