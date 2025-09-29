(let ([i (- 1)])
  (begin
    (while (> i 0)
           (begin
             (set! i 10)
             1))
    i))
