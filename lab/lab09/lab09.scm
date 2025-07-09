(define (over-or-under num1 num2) 
  (cond 
    ((< num1 num2) -1)
    ((= num1 num2) 0)
    ((> num1 num2) 1)
  )
)

(define (make-adder num) 
  (define (f inc)
    (+ num inc))
  f
)

(define (composed f g) 
  (define (hh x)
    (f (g x))
  )
  hh
)

(define (repeat f n) 
  (if (= n 1) 
    f  
    (composed f (repeat f (- n 1)))
  )
)

(define (max a b)
  (if (> a b)
      a
      b))

(define (min a b)
  (if (> a b)
      b
      a))

(define (gcd a b)
  (if (= (min a b) a)
    (gcd b a)
    )
  (if (zero? b)
    a
    (gcd b (modulo a b) )
  )
)
