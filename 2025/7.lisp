(defun parse-input (input)
  (let ((input (uiop:read-file-lines input)))
    input))

(defun print-input (input)
  (loop for line in input
        do (print line)))

(defun get-x-y (x y input)
  (aref (nth y input) x))

(defun part1 (input)
  (loop with input = (parse-input input)
        with input-length = (length input)
        with split = 0

        for line in input
        for row from 0 to (- input-length 2)

        do (loop for c across line
                 for column from 0

                 do (cond ((and (or (char= c #\S)
                                    (char= c #\|))
                                (not (char= (get-x-y column
                                                     (1+ row)
                                                     input)
                                            #\^)))
                           (setf (aref (nth (1+ row) input) column) #\|))
                          ((and (char= c #\|)
                                (char= (get-x-y column (1+ row) input) #\^))
                           (progn (when (not (char= (get-x-y (1- column)
                                                             (1+ row)
                                                             input)
                                                    #\^))
                                    (setf (aref (nth (1+ row)
                                                     input)
                                                (1- column))
                                          #\|))
                                  (when (not (char= (get-x-y (1+ column)
                                                             (1+ row)
                                                             input)
                                                    #\^))
                                    (setf (aref (nth (1+ row)
                                                     input)
                                                (1+ column))
                                          #\|))
                                  (setf split (1+ split))))))

        finally (return split)))
