(require 'cl-ppcre)


(defun do-mul (input)
  (let* ((numbers (cl-ppcre:all-matches-as-strings "[0-9]+" input))
	 (a (parse-integer (nth 0 numbers)))
	 (b (parse-integer (nth 1 numbers)))
	 (result (* a b)))
    (declare (type fixnum a b result))
    result))


(defun find-mul (input)
  (cl-ppcre:all-matches-as-strings "mul\\([0-9]+,[0-9]+\\)" input))


(defun part1 (input)
  (loop with result fixnum = 0
	for i in (find-mul input)
	for j = (do-mul i)
	do (incf result j)
	finally (return result)))


(defun remove-dont (input)
  (cl-ppcre:regex-replace-all "don't\\(\\).*?(do\\(\\)|$)" input ""))


(defun part2 (input)
  (loop with result fixnum = 0
	for i in (find-mul (remove-dont input))
	for j = (do-mul i)
	do (incf result j)
	finally (return result)))


(defvar *input* (apply #'concatenate 'string
		       (uiop:read-file-lines "3.input")))
