(let ([t (read)])
  (let ([i 0])
    (begin
      (while (not (eq? i t))
             (if (< t 0)
                 (set! i (- i 1))
                 (set! i (+ i 1))))
      i)))
