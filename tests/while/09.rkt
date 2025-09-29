(let ([x (read)])
  (let ([y 9])
    (begin
      (if (read)
          (set! y (+ y x))
          (set! y (- y x)))
      (eq? 10 y))))
