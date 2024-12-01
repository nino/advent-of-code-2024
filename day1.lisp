(ql:quickload 'arrow-macros)
(ql:quickload 'uiop)
(ql:quickload 'alexandria)
(ql:quickload 'str)
(defpackage :day1
  (:use :cl :arrow-macros))

(in-package :day1)

(setq *lines* (uiop:read-file-lines "day1-input.txt"))

(let ((pairs (mapcar #'str:words *lines*)))
  (let ((left (mapcar (lambda (pair)
                        (parse-integer (first pair)))
                      pairs))
        (right (mapcar (lambda (pair)
                         (parse-integer (second pair)))
                       pairs)))
    (loop for i in (mapcar (lambda (l r) (abs (- l r)))
                           (sort left #'<)
                           (sort right #'<))
          summing i)))

