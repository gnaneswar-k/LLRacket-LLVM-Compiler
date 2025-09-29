(let ([i (read)])
  (if (< i 0)
      (set! i (- i 1))
      (set! i (+ i 1))))
