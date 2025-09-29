(let ([i 5])
  (begin
    (while (> i 0)
           (begin
             (set! i (- i 1))
             i))
    i))
