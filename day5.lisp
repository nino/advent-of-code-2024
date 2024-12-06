(defpackage :day5
  (:use :cl))

(in-package :day5)

(defun prep (input)
  (destructuring-bind
      (rules updates)
      (mapcar #'str:lines (ppcre:split "\\n\\n" input))
    (list (mapcar (lambda (rule-str)
                    (mapcar #'parse-integer (str:split "|" rule-str)))
                  rules)
          (mapcar (lambda (update-str)
                    (mapcar #'parse-integer (str:split "," update-str)))
                  updates))))

(prep (uiop:read-file-string "day5-test.txt"))

(defun update-valid-p (rules update)
  (let ((indices (indices update)))
    (flet ((rule-fulfilled-p (rule)
             (let ((index1 (gethash (first rule) indices))
                   (index2 (gethash (second rule) indices)))
               (or (null index1)
                   (null index2)
                   (< index1 index2)))))
      (loop for rule in rules
            always (rule-fulfilled-p rule)))))

(defun filter-valid-updates (rules updates)
  (remove-if-not (lambda (update) (update-valid-p rules update)) updates))

(defun indices (seq)
  (let ((indices (make-hash-table))
        (index 0))
    (loop for num in seq
          do (progn (setf (gethash num indices) index)
                    (incf index)))
    indices))

(defun middle-page (update)
  (nth (truncate (/ (length update) 2)) update))

(defun part1 (input)
  (destructuring-bind (rules updates) (prep input)
    (loop for update in (filter-valid-updates rules updates)
          sum (middle-page update))))

(part1 (uiop:read-file-string "day5-input.txt"))
