(let ([t (vector 2 #t (vector 4 #f))])
  (begin
   (vector-set! t 1 #f)
   (vector-ref t 1)))
