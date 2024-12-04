; God this is ugly code
(ql:quickload 'uiop)
(ql:quickload 'str)
(ql:quickload "cl-ppcre")
(defpackage :day4
  (:use :cl))

(in-package :day4)

(defun prep (input)
  "List of strings -> Array of strings"
  (coerce input 'vector))

(defun matref (matrix row col)
  (let ((width  (length (aref matrix 0)))
        (height (length matrix)))
    (if (and (<= 0 row (1- height))
             (<= 0 col (1- width)))
        (aref (aref matrix row) col)
        #\.)))

(defun coord+ (a b)
  (mapcar #'+ a b))

(defun coord* (a fac)
  (mapcar (lambda (component) (* component fac)) a))

(defun get-string (matrix start dir len)
  (coerce (loop for i below len
                collect (let ((coord (coord+ start (coord* dir i))))
                          (matref matrix (first coord) (second coord))))
          'string))

(defun part1 (input)
  (let ((prepped (prep input)))
    (loop for dir in '((-1 -1) (-1 0) (-1 1) (0 -1 ) (0 1) (1 -1) (1 0) (1 1))
          sum (loop for row below (length prepped)
                    sum (loop for col below (length (aref prepped 0))
                              count (equal "XMAS"
                                           (get-string prepped
                                                       (list row col)
                                                       dir
                                                       4)))))))

(part1 (uiop:read-file-lines "day4-input.txt"))

(defun get-x (matrix row col)
  (list (get-string matrix (list (1- row) (1- col)) '(1 1) 3)
        (get-string matrix (list (1+ row) (1- col)) '(-1 1) 3)))

(defun is-xmas-x (matrix row col)
  (let ((x (get-x matrix row col)))
    (and (or (equal (first x) "MAS")
             (equal (first x) "SAM"))
         (or (equal (second x) "MAS")
             (equal (second x) "SAM")))))

(defun part2 (input)
  (let ((prepped (prep input)))
    (loop for row from 1 below (1- (length prepped))
          sum (loop for col from 1 below (1- (length (aref prepped 0)))
                    count (is-xmas-x prepped row col)))))


(part2 (uiop:read-file-lines "day4-input.txt"))
