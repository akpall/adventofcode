(defun parse-input (input)
  (let ((input (uiop:read-file-lines input)))
    (loop for i in input
          collect (loop with rotation
                        with distance

                        for c across i

                        if (or (char= c #\L)
                               (char= c #\R))
                          do (setf rotation c)
                        else
                          do (setf distance (concatenate 'string
                                                         distance
                                                         (string c)))

                        finally (return (list rotation
                                              (parse-integer distance)))))))

(defun part1 (input)
  (loop with dial = 50
        with dial-points-at-0 = 0

        for i in (parse-input input)
        for rotation = (first i)
        for distance = (second i)

        if (char= rotation #\L)
          do (setf dial (mod (- dial distance) 100))

        if (char= rotation #\R)
          do (setf dial (mod (+ dial distance) 100))

        if (= dial 0)
          do (setf dial-points-at-0 (1+ dial-points-at-0))

        finally (return dial-points-at-0)))

(defun part2 (input)
  (loop with dial-start = 50
        with dial-end = 0
        with dial-points-at-0 = 0

        for i in (parse-input input)

        for rotation = (first i)
        for distance = (second i)
        for distance-mod = (mod distance 100)
        for distance-clicks = (/ (- distance distance-mod) 100)
        do (setf dial-points-at-0 (+ dial-points-at-0 distance-clicks))

        if (char= rotation #\L)
          do (setf dial-end (mod (- dial-start distance-mod) 100))
          and do (if (and (> dial-end dial-start)
                          (not (= dial-start 0)))
                     (setf dial-points-at-0 (1+ dial-points-at-0)))

        if (char= rotation #\R)
          do (setf dial-end (mod (+ dial-start distance-mod) 100))
          and do (if (and (< dial-end dial-start)
                          (not (= dial-end 0)))
                     (setf dial-points-at-0 (1+ dial-points-at-0)))

        if (= dial-end 0)
          do (setf dial-points-at-0 (1+ dial-points-at-0))

        do (setf dial-start dial-end)

        finally (return dial-points-at-0)))
