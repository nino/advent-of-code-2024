(ql:quickload 'arrow-macros)
(ql:quickload 'uiop)
(ql:quickload 'alexandria)
(ql:quickload 'str)
(defpackage :day1
  (:use :cl :arrow-macros))

(in-package :day1)

(defun prep (input-file)
  (let* ((lines (uiop:read-file-lines "day1-input.txt"))
         (pairs (mapcar #'str:words lines))
         (left (mapcar (lambda (pair)
                         (parse-integer (first pair)))
                       pairs))
         (right (mapcar (lambda (pair)
                          (parse-integer (second pair)))
                        pairs)))
    (list left right)))

; Part 1
(defun part1 ()
  (let* ((prepped (prep "day1-input.txt")))
    (loop for i in (mapcar (lambda (l r) (abs (- l r)))
                           (sort (first prepped) #'<)
                           (sort (second prepped) #'<))
          summing i)))
(part1)

; Part 2
(defun part2 ()
  (let* ((prepped (prep "day1-input.txt")))
    (loop for l in (first prepped)
          summing (* l (count l (second prepped))))))
(part2)
