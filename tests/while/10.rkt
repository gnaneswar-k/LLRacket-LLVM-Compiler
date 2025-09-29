(begin
  (let ([r 11])
    (while (> r 9)
           (begin
             (set! r (- (read) 1))
             (eq? r 10)))))
