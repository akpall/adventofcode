(declaim (optimize (speed 3)))


(defun calculate-checksum (input)
  (loop for id in input
        for pos fixnum from 0
        if (numberp id)
          sum (the fixnum (* pos (the fixnum id))) fixnum))


(defun move-blocks (input)
  (declare (type list input))
  (loop with len = (length input)
        for i from 0 below len
        for c = (nth i input)
        for last-pos = (position-if #'numberp input :from-end t)
        for last-value = (nth last-pos input)
        if (and (equal c #\.)
                (> last-pos i))
           do (setf (nth i input) last-value)
              (setf (nth last-pos input) #\.))
  input)

(defun parse-blocks (input)
  (declare (type simple-string input))
  (loop with id fixnum = 0
        for c across input
        for count = (digit-char-p c)
        for i fixnum from 0
        if (evenp i)
          nconc (loop repeat count
                      collect id)
          and do (incf id)
        else
          nconc (loop repeat count
                      collect #\.)))


(defun part1 (input-file)
  (with-open-file (s input-file)
    (let* ((input (read-line s))
           (parsed-blocks (parse-blocks input))
           (moved-blocks (move-blocks parsed-blocks))
           (checksum (calculate-checksum moved-blocks)))
      checksum)))
