(defun part1 (input-file)
  (with-open-file (s input-file)
    (multiple-value-bind (antennas max-x max-y) (parse-input-stream s)
      (let* ((alist (list-to-alist antennas))
	     (combinations (loop for i in alist
				 nconc (do-combinations (second i))))
	     (antinodes (loop with antinode
			      for i in combinations
			      for antenna-b = (first i)
			      for antenna-c = (second i)
			      for diff-x = (- (first antenna-b) (first antenna-c))
			      for diff-y = (- (second antenna-b) (second antenna-c))
			      for antenna-a = (list (+ (first antenna-b) diff-x) (+ (second antenna-b) diff-y))
			      for antenna-d = (list (- (first antenna-c) diff-x) (- (second antenna-c) diff-y))
			      if (and (<= 0 (first antenna-a) max-x)
				      (<= 0 (second antenna-a) max-y))
				do (push antenna-a antinode)
			      if (and (<= 0 (first antenna-d) max-x)
				      (<= 0 (second antenna-d) max-y))
				do (push antenna-d antinode)
			      finally (return antinode))))
	(length (remove-duplicates antinodes :test #'equal))))))


(defun parse-input-stream (stream)
  (loop with max-x = 0
	with max-y = 0
	with antennas
	for line = (read-line stream nil)
	while line
	for y from 0
	do (setf max-y (max max-y y))
	   (loop for c across line
		 for x from 0
		 do (setf max-x (max max-x x))
		 when (not (char= c #\.))
		   do (push (list c (list x y)) antennas))
	finally (return (values antennas max-x max-y))))


(defun list-to-alist (input)
  (loop with output
	for entry in input
	for c = (first entry)
	for pos = (second entry)
	for assoc = (second (assoc c output))
	if assoc
	  do (setf (second (assoc c output)) (append assoc (list pos)))
	else
	  do (push (list c (list pos)) output)
	finally (return output)))


(defun do-combinations (list)
  (loop for i in list
	for cdr = (cdr list)
	if cdr
	  nconc (loop for j in cdr
		      collect (list i j))
	and do (pop list)))
