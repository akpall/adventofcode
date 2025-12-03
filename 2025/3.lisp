(defun parse-input (input)
  (let ((read-input (uiop:read-file-lines input)))
    read-input))

(defun part1 (input)
  (loop with parsed-input = (parse-input input)

        for bank in parsed-input
        for joltage = (loop for c across bank
                            for i from 1
                            for bank-subseq = (subseq bank i)
                            for joltage = (loop for c across bank-subseq
                                                collect (parse-integer (string c)))

                            if joltage
                              collect (format nil "~d~d" c (apply 'max joltage)))

        sum (apply 'max (mapcar 'parse-integer joltage))))
