(require 'cl-ppcre)


(declaim (ftype (function (list) fixnum) scan-diagonal))
(defun scan-diagonal (input)
  (destructuring-bind (a b c d) input
    (declare (type simple-string a b c d))
    (loop with result fixnum = 0
	  with lines = (length input)
	  with length = (length a)
	  for i from 0 to (- length lines)
	  do (if (or (and (char= (char a i) #\X)
			  (char= (char b (+ i 1)) #\M)
			  (char= (char c (+ i 2)) #\A)
			  (char= (char d (+ i 3)) #\S))
		     (and (char= (char a i) #\S)
			  (char= (char b (+ i 1)) #\A)
			  (char= (char c (+ i 2)) #\M)
			  (char= (char d (+ i 3)) #\X)))
		 (incf result))
	     (if (or (and (char= (char a (+ i 3)) #\X)
			  (char= (char b (+ i 2)) #\M)
			  (char= (char c (+ i 1)) #\A)
			  (char= (char d i) #\S))
		     (and (char= (char a (+ i 3)) #\S)
			  (char= (char b (+ i 2)) #\A)
			  (char= (char c (+ i 1)) #\M)
			  (char= (char d i) #\X)))
		 (incf result))
	  finally (return result))))


(declaim (ftype (function (string) fixnum) scan-horizontal))
(defun scan-horizontal (input)
  (let ((xmas (cl-ppcre:count-matches "XMAS" input))
	(samx (cl-ppcre:count-matches "SAMX" input)))
    (declare (type fixnum xmas samx))
    (+ xmas samx)))


(defun scan-vertical (input)
  (destructuring-bind (a b c d) input
    (declare (type simple-string a b c d))
    (loop for i from 0 below (length a)
	  count (or (and (char= (char a i) #\X)
			 (char= (char b i) #\M)
			 (char= (char c i) #\A)
			 (char= (char d i) #\S))
		    (and (char= (char a i) #\S)
			 (char= (char b i) #\A)
			 (char= (char c i) #\M)
			 (char= (char d i) #\X))))))


(defun part1 (input)
  (loop with result fixnum = 0
	for i in input
	for a = nil then b
	for b = nil then c
	for c = nil then d
	for d = i
	do (incf result (scan-horizontal d))
	   (when (and a b c d)
		 (incf result (+ (scan-vertical (list a b c d))
				 (scan-diagonal (list a b c d)))))
	finally (return result)))


(defun scan-x-mas (input)
  (destructuring-bind (a b c) input
    (declare (type simple-string a b c))
    (loop with result fixnum = 0
	  with length = (length (the simple-string (nth 0 input)))
	  for i from 1 to (- length 2)
	  for j = (char b i)
	  when (and (char= j #\A)
		    (and (or (and (char= (char a (1- i)) #\M) (char= (char c (1+ i)) #\S))
			     (and (char= (char a (1- i)) #\S) (char= (char c (1+ i)) #\M)))
			 (or (and (char= (char a (1+ i)) #\M) (char= (char c (1- i)) #\S))
			     (and (char= (char a (1+ i)) #\S) (char= (char c (1- i)) #\M)))))
	    do (incf result)
	  finally (return result))))


(defun part2 (input)
  (loop with result fixnum = 0
	for (a b c) on input by #'cdr
	do (if (and a b c)
	       (incf result (scan-x-mas (list a b c))))
	finally (return result)))


(defvar *input* (uiop:read-file-lines "4.input"))
