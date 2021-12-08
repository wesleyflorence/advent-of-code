(ql:quickload :cl-ppcre)
(ql:quickload :split-sequence)

;;; Read in the file
(defun read-input (file)
  (with-open-file (in file)
    (loop
      for line = (read-line in nil)
      while line
      collect (mapcar #'parse-integer
                      (cl-ppcre:all-matches-as-strings "(\\d+)" line)))))

(defun parse-boards (input)
  (split-sequence:split-sequence-if #'null input))

(defun get-vertical-rows (board)
  (reduce (lambda (a b)
            (map 'list #'(lambda (f s)
                           (if (consp f)
                               (append f (list s))
                               (cons f (cons s nil))))
                 a b))
          board))

(defun add-vertical-row-to-boards (boards)
  (loop for board in boards
        collect (append board (get-vertical-rows board))))

(defparameter *name* "04.input")
(defparameter *moves* (first (read-input *name*)))
(defparameter *boards* (parse-boards (cddr (read-input *name*))))
(defparameter *boards-with-verts* (add-vertical-row-to-boards *boards*))

;;; Part A
(defun filter-move (move board)
  (mapcar (lambda (row)
            (remove move row))
          board))

(defun play-move (move boards)
  (loop for board in boards
       collect (filter-move move board)))

(defun get-winning-board-total (boards)
  (loop for board in boards
        when (some #'null board)
        collect (reduce (lambda (a b)
                         (+ a (reduce #'+ b))) (subseq board 0 5)
                         :initial-value 0)))

(defun check-for-win (move boards)
  (let ((winner (get-winning-board-total boards)))
    (if winner
        (* move (car winner)))))

(defun play (moves boards)
  (loop for move in moves
        do (setf boards (play-move move boards))
        when (check-for-win move boards) return it))

;;; Part B
(defun find-last-winner (moves boards)
  (loop for move in moves
        ))

;;; Solutions
(format t "Problem 04 A: ~a~%" (play *moves* *boards-with-verts*))
;(format t "Problem 04 B: ~a~%")
