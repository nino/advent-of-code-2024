(ql:quickload 'uiop)
(ql:quickload 'str)
(defpackage :day2
  (:use :cl))

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


(defun without (idx seq)
  (concatenate 'list
               (subseq seq 0 idx)
               (subseq seq (1+ idx))))


(defun report-safeish? (report)
  (or (report-safe? report)
      (loop for i below (length report)
            when (report-safe? (without i report))
            return t)))

; Part 1
(defun part1 (filename)
  (let ((reports (read-reports filename)))
    (count-if #'report-safe? reports)))
(part1 "day2-input.txt")


(defun part2 (filename)
  (let ((reports (read-reports filename)))
    (count-if #'report-safeish? reports)))
(part2 "day2-input.txt")
