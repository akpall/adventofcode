(require :cl-ppcre)

(defvar *input-file* "5.input")


(defun update-correctness-p (page-ordering-rules update)
  (loop for i in update
	for j from 0
	for subseq = (subseq update 0 j)
	for assoc = (assoc i page-ordering-rules)
	for intersection = (intersection subseq (second assoc))
	never intersection))


(defun correct-update (page-ordering-rules update)
  (if (update-correctness-p page-ordering-rules update)
      update
      (loop for i in update
	    for j from 0
	    for subseq = (subseq update 0 j)
	    for assoc = (assoc i page-ordering-rules)
	    for subseq-after = (subseq update (1+ j))
	    for tmp = (car (last subseq))
	    for intersection = (intersection subseq (second assoc))
	    if (and subseq
		      intersection)
	      do (setf (car (last subseq)) i)
		 (return (correct-update page-ordering-rules (append subseq (list tmp) subseq-after))))))

(defun find-correct-updates (page-ordering-rules updates)
  (loop for update in updates
	when (update-correctness-p page-ordering-rules update)
	  collect update))


(defun get-middle-value (list)
  (nth (/ (1- (length list))
	  2)
       list ))


(defun make-page-ordering-rules (rules)
  (loop with output
	for rule in rules
	for before = (first rule)
	for after = (second rule)
	for find-before = (find before output :key #'car)
	do (if find-before
	       (push after (second find-before))
	       (push (list before (list after)) output))
	finally (return output)))


(defun parse-file (stream)
  (loop with page-ordering-rules
	with updates
	with page-ordering-rules-section = t
	for line = (read-line stream nil)
	while line
	do (if (not page-ordering-rules-section)
	       (push line updates)
	       (if (equal line "")
		   (setf page-ordering-rules-section nil)
		   (push line page-ordering-rules)))
	finally (return (values page-ordering-rules updates))))


(defun parse-page-ordering-rule (rule)
  (mapcar #'parse-integer (cl-ppcre:split "\\|" rule)))


(defun parse-update (update)
  (mapcar #'parse-integer (cl-ppcre:split "," update)))


(defun part1 (file)
  (with-open-file (s file)
    (multiple-value-bind (page-ordering-rules updates) (parse-file s)
      (let* ((parsed-page-ordering-rules (make-page-ordering-rules (mapcar #'parse-page-ordering-rule
									   page-ordering-rules)))
	     (parsed-updates (mapcar #'parse-update updates))
	     (correct-updates (find-correct-updates parsed-page-ordering-rules parsed-updates)))
	(reduce #'+ (mapcar #'get-middle-value correct-updates))))))


(defun part2 (file)
  (with-open-file (s file)
    (multiple-value-bind (page-ordering-rules updates) (parse-file s)
      (let* ((parsed-page-ordering-rules (make-page-ordering-rules (mapcar #'parse-page-ordering-rule
									   page-ordering-rules)))
	     (parsed-updates (mapcar #'parse-update updates))
	     (correct-updates (find-correct-updates parsed-page-ordering-rules parsed-updates))
	     (incorrect-updates (set-exclusive-or parsed-updates correct-updates))
	     (corrected-updates (mapcar #'(lambda (x) (correct-update parsed-page-ordering-rules x))
					incorrect-updates)))
	(reduce #'+ (mapcar #'get-middle-value corrected-updates))))))
