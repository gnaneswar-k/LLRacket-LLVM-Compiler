(let ([t (vector 2 #t)])
  (begin
   (vector-set! t 1 #f)
   (vector-ref t 1)))
