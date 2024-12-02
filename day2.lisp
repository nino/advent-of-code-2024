(ql:quickload 'arrow-macros)
(ql:quickload 'uiop)
(ql:quickload 'alexandria)
(ql:quickload 'str)
(defpackage :day2
  (:use :cl :arrow-macros))

(in-package :day2)

(defun read-reports (filename)
  (mapcar (lambda (line)
            (mapcar #'parse-integer (str:words line)))
          (uiop:read-file-lines filename)))

(defun pairs (report)
  (if (and (first report) (second report))
      (cons (list (first report) (second report))
            (pairs (rest report)))
      nil))

(defun all= (items)
  (every (lambda (item) (= item (first items)))
         items))

(defun report-safe? (report)
  (let ((diffs (mapcar (lambda (pair) (apply #'- pair)) (pairs report))))
    (and (all= (mapcar #'signum diffs))
         (every (lambda (diff)
                  (<= 1 (abs diff) 3))
                diffs))))

(report-safe? '(1 2 5))

; Part 1
(defun part1 (filename)
  (let ((reports (read-reports filename)))
    (count-if #'report-safe? reports)))
(part1 "day2-input.txt")
