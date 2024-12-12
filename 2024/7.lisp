(require :cl-ppcre)

(defvar *file-name* "7.input")


(defun add-or-multiply-part1 (list)
  (let ((first (car list))
	(second (cadr list)))
    (if (and first second)
	(list (add-or-multiply-part1 (cons (+ first second) (cddr list)))
	      (add-or-multiply-part1 (cons (* first second) (cddr list))))
	first)))


(defun add-or-multiply-part2 (list)
  (let ((first (car list))
	(second (cadr list)))
    (if (and first second)
	(list (add-or-multiply-part2 (cons (+ first second) (cddr list)))
	      (add-or-multiply-part2 (cons (* first second) (cddr list)))
	      (add-or-multiply-part2 (cons (parse-integer (format nil "~a~a" first second)) (cddr list))))
	first)))


(defun find-value-in-nested (value tree)
  (cond ((eql tree value) t)
	((atom tree) nil)
	(t (or (find-value-in-nested value (car tree))
	       (find-value-in-nested value (cdr tree))))))


(defun parse-input (stream)
  (loop for line = (read-line stream nil)
	while line
	collect (let* ((split (cl-ppcre:split ":" line))
		       (values (cl-ppcre:all-matches-as-strings "\\d+" (second split))))
		  (list (parse-integer (first split))
			(mapcar #'parse-integer values)))))


(defun part1 ()
  (with-open-file (s *file-name*)
    (loop for (sum values) in (parse-input s)
	  sum (if (find-value-in-nested sum (add-or-multiply-part1 values))
		  sum
		  0))))


(defun part2 ()
  (with-open-file (s *file-name*)
    (loop for (sum values) in (parse-input s)
	  sum (if (find-value-in-nested sum (add-or-multiply-part2 values))
		  sum
		  0))))
