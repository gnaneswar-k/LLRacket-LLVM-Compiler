(let ([t (vector 2 #t (vector 4 #f))])
  (begin
   (vector-set! (vector-ref t 2) 0 6)
   (vector-ref (vector-ref t 2) 0)))
