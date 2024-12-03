(ql:quickload 'uiop)
(ql:quickload 'str)
(ql:quickload "cl-ppcre")
(defpackage :day3
  (:use :cl))

(in-package :day3)



(defun calc-ops (ops)
  (loop for pair in ops
        sum (* (first pair) (second pair))))

(defun parse-op (op-string)
  (mapcar (lambda (n) (parse-integer n :junk-allowed t))
          (str:split "," (str:replace-all "mul(" "" op-string))))

(defun part1 (input)
  (let ((ops (ppcre:all-matches-as-strings "mul\\((\\d+),(\\d+)\\)" input)))
    (calc-ops (mapcar #'parse-op ops))))

(part1 (uiop:read-file-string "day3-input.txt"))

(part1 "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")
