(begin
  (let ([r 11])
    (begin
      (set! r (and (read) (>= r 10)))
      r)))
