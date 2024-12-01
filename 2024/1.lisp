(defun split-groups (input)
  (let* ((first (read-from-string input))
	 (second (read-from-string (subseq input (length (write-to-string first))))))
    (list first second)))


(defun parse-input (input)
  (loop with group1 = ()
	with group2 = ()
	for i in input
	for j = (split-groups i)
	for first = (nth 0 j)
	for second = (nth 1 j)
	do (push first group1)
	   (push second group2)
	finally (return (list group1 group2))))


(defun part1 (input)
  (loop with result fixnum = 0
	for i fixnum in (nth 0 input)
	for j fixnum in (nth 1 input)
	for distance = (abs (- i j))
	do (incf result distance)
	finally (return result)))


(defun part2 (input)
  (loop with result fixnum = 0
	for i fixnum in (nth 0 input)
	for j fixnum = (count i (the list (nth 1 input)))
	for similarity fixnum = (* i j)
	do (incf result similarity)
	finally (return result)))


(defun sort-groups (input)
  (let* ((first-group (nth 0 input))
	 (second-group (nth 1 input))
	 (first-sorted (sort (the list first-group) #'<))
	 (second-sorted (sort (the list second-group) #'<)))
    (list first-sorted second-sorted)))


(defvar *input* (sort-groups (parse-input (uiop:read-file-lines "1.input"))))
