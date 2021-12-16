(in-package :advent)

(defun read-input ()
  (mapcar (lambda (s)
            (destructuring-bind (signals codes)
                (cl-ppcre:split "( \\| )" s :omit-unmatched-p t)
              (list (cl-ppcre:split "\\s+" signals)
                    (cl-ppcre:split "\\s+" codes))))
          (uiop:read-file-lines "input/08.input")))

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

(defun d8/summary ()
  (format t "Problem 08 A: ~a~%" (get-total-known-codes))
  (format t "Problem 08 B: ~a~%" "todo"))
