(require :cl-ppcre)

(defun parse-input (input)
  (let ((input (uiop:read-file-lines input)))
    (loop with database = t
          with id-range-list
          with id-list

          for line in input

          if (string= line "")
            do (setf database nil)
          else
            if database
              do (push (mapcar 'parse-integer (cl-ppcre:split "-" line)) id-range-list)
          else
            do (push (parse-integer line) id-list)

          finally (return (list id-range-list id-list)))))

(defun id-fresh-p (id id-range-list)
  (loop for id-range in id-range-list
        for start = (first id-range)
        for end = (second id-range)
        if (<= start id end)
          return t))

(defun part1 (input)
  (let* ((input (parse-input input))
         (id-range-list (first input))
         (id-list (second input)))
    (loop for id in id-list
          if (id-fresh-p id id-range-list)
            count t)))
