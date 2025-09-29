(let ([x (let ([x 1]) (+ x 2))]) (+ (let ([x (+ (let ([x 3]) (+ x 4)) 5)]) x) x))
