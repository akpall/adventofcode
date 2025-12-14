(require :cl-ppcre)

(defun parse-input (input)
  (loop with input = (uiop:read-file-lines input)
        with input-length = (length input)
        with output

        for line in input
        for i from 1
        for parsed-line = (if (= i input-length)
                              (cl-ppcre:all-matches-as-strings "[\\+\\-\\*\\/]"
                                                             line)
                              (mapcar 'parse-integer
                                      (cl-ppcre:all-matches-as-strings "\\d+"
                                                                       line)))
        do (setf output (append (list parsed-line) output))

        finally (return output)))

(defun turn-input (input)
  (loop for row from 0 to (1- (length (nth 0 input)))
        collect (loop for line from 0 to (1- (length input))
                      collect (nth row (nth line input)))))

(defun part1 (input)
  (loop with input = (parse-input input)
        with input-turned = (turn-input input)

        for line in input-turned
        for symbol = (nth 0 line)
        for problem = (subseq line 1)

        sum (apply (intern symbol) problem)))
