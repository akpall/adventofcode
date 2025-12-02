(require :cl-ppcre)

(defun parse-input (input)
  (let ((read-input (ppcre:all-matches-as-strings "(\\d+)-(\\d+)"
                                                  (uiop:read-file-string input))))
    (loop for i in read-input
          for j = (ppcre:split "-" i)
          for k = (map 'list 'parse-integer j)
          collect k)))

(defun part1-invalid-id-p (id)
  (let* ((id-string (write-to-string id))
         (id-string-length-half (floor (/ (length id-string) 2))))
    (when (string= (subseq id-string 0 id-string-length-half)
                 (subseq id-string id-string-length-half))
        t)))

(defun part1 (input)
  (loop for (start end) in (parse-input input)
        sum (loop for id from start to end
                  when (part1-invalid-id-p id)
                    sum id)))

(defun part2-invalid-ID-p (id)
  (loop with id-string = (write-to-string id)
        with id-string-length = (length id-string)

        for pattern-length from 1 to (/ id-string-length 2)
        when (and (zerop (mod id-string-length pattern-length))
                  (loop for i from pattern-length below id-string-length by pattern-length
                        always (string= (subseq id-string 0 pattern-length)
                                        (subseq id-string i (+ i pattern-length)))))
          return t))

(defun part2 (input)
  (loop for (start end) in (parse-input input)
        sum (loop for id from start to end
                  when (part2-invalid-id-p id)
                    sum id)))
