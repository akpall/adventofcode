(defvar *input-file* "6.input")

(defun find-path (obstructions guard map-x map-y)
  (destructuring-bind (x y direction) guard
    (loop while (and (<= 0 x map-x)
		     (<= 0 y map-y))
	  collect (list x y)
	  do (case direction
	       (up
		(if (find (list x (1- y)) obstructions :test #'equal)
		    (setf direction 'right)
		    (decf y)))
	       (down
		(if (find (list x (1+ y)) obstructions :test #'equal)
		    (setf direction 'left)
		    (incf y)))
	       (left
		(if (find (list (1- x) y) obstructions :test #'equal)
		    (setf direction 'up)
		    (decf x)))
	       (right
		(if (find (list (1+ x) y) obstructions :test #'equal)
		    (setf direction 'down)
		    (incf x)))))))


(defun parse-input-stream (stream)
  (loop with obstructions
	with guard
	for line = (read-line stream nil)
	for length = (length line) then (max length (length line))
	for y from 0
	while line
	do (loop for c across line
		 for x from 0
		 when (char= c #\#)
		   do (push (list x y) obstructions)
		 when (char= c #\^)
		   do (setf guard (list x y 'up)))
	finally (return (values obstructions guard (1- length) (1- y)))))


(defun part1 (input-file)
  (with-open-file (s input-file)
    (multiple-value-bind (obstructions guard map-x map-y) (parse-input-stream s)
      (let ((path (find-path obstructions guard map-x map-y)))
	(length (remove-duplicates path :test #'equal))))))


(defun part2 (input-file)
  (with-open-file (s input-file)
    (multiple-value-bind (obstructions guard map-x map-y) (parse-input-stream s)
      (let ((new-obstructions (find-pos-for-obstructions obstructions guard map-x map-y)))
	(length (remove-duplicates new-obstructions :test #'equal))))))


(defun find-pos-for-obstructions (obstructions guard map-x map-y)
  (destructuring-bind (x y direction) guard
    (loop with new-obstructions
	  while (and (<= 0 x map-x)
		     (<= 0 y map-y))
	  collect (list x y) into path
	  do (case direction
	       (up
		(if (and (not (find (list x (1- y)) obstructions :test #'equal))
			 (loop named obs
			       for obs-x from x below map-x
			       if (and (find (list obs-x y) path :test #'equal)
				       (or (find (list (1+ obs-x) y) obstructions :test #'equal)
					   (= (1+ obs-x) map-x)))
				 do (return-from obs t)))
		    (push (list x (1- y)) new-obstructions)))
	       (down
		(if (and (not (find (list x (1+ y)) obstructions :test #'equal))
			 (loop named obs
			       for obs-x from x above 0
			       if (and (find (list obs-x y) path :test #'equal)
				       (or (find (list (1- obs-x) y) obstructions :test #'equal)
					   (= (1- obs-x) 0)))
				 do (return-from obs t)))
		    (push (list x (1+ y)) new-obstructions)))
	       (left
		(if (and (not (find (list (1- x) y) obstructions :test #'equal))
			 (loop named obs
			       for obs-y from y above 0
			       if (and (find (list x obs-y) path :test #'equal)
				       (or (find (list x (1- obs-y)) obstructions :test #'equal)
					   (= (1- obs-y) 0)))
				 do (return-from obs t)))
		    (push (list (1- x) y) new-obstructions)))
	       (right
		(if (and (not (find (list (1+ x) y) obstructions :test #'equal))
			 (loop named obs
			       for obs-y from y below map-y
			       if (and (find (list x obs-y) path :test #'equal)
				       (or (find (list x (1+ obs-y)) obstructions :test #'equal)
					   (= (1+ obs-y) map-y)))
				 do (return-from obs t)))
		    (push (list (1+ x) y) new-obstructions))))
	  do (case direction
	       (up
		(if (find (list x (1- y)) obstructions :test #'equal)
		    (setf direction 'right)
		    (decf y)))
	       (down
		(if (find (list x (1+ y)) obstructions :test #'equal)
		    (setf direction 'left)
		    (incf y)))
	       (left
		(if (find (list (1- x) y) obstructions :test #'equal)
		    (setf direction 'up)
		    (decf x)))
	       (right
		(if (find (list (1+ x) y) obstructions :test #'equal)
		    (setf direction 'down)
		    (incf x))))
	  finally (return new-obstructions))))
