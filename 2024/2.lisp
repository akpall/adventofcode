(defun decreasingp (input)
  (loop for (a b) of-type (fixnum (or fixnum null)) on input by #'cdr
	while b
	for diff = (- b a)
	always (> 0 diff -4)))


(defun increasingp (input)
  (loop for (a b) of-type (fixnum (or fixnum null)) on input by #'cdr
	while b
	for diff = (- b a)
	always (< 0 diff 4)))


(defun analyze-data (input)
  (or (increasingp input)
      (decreasingp input)))


(defun input-string-to-list (input)
  (with-input-from-string (s input)
    (loop for num = (read s nil)
	  while num
	  collect num)))


(defun part1 (input)
  (count-if
   (lambda (x)
     (analyze-data (input-string-to-list x)))
   input))


(declaim (ftype (function (list fixnum) list) remove-nth))
(defun remove-nth (input n)
  (if (< (1- n) 0)
      (subseq input 1)
      (append (subseq input 0 (1- n))
	      (subseq input n))))


(declaim (ftype (function (list) boolean) safe-analyze-data))
(defun safe-analyze-data (input)
  (or (analyze-data input)
      (loop for i from 0 to (length input)
	    for j = (remove-nth input i)
	    thereis (or (increasingp j)
			(decreasingp j)))))


(defun part2 (input)
  (count-if
   (lambda (x)
     (safe-analyze-data (input-string-to-list x)))
   input))


(defvar *input* (uiop:read-file-lines "2.input"))
