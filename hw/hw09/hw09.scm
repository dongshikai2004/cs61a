(define (curry-cook formals body) 
  (define (f formal)
    (if (null? formal)
      body
      (cons `lambda (list (cons (car formal) nil)(f(cdr formal))))
    )
  )
  (f formals)
)

(define (curry-consume curry args)
  (cond 
    ((null? args) curry)
    (else (curry-consume (curry(car args)) (cdr args)))
  )
)

(define-macro (switch expr options)
  (switch-to-cond (list 'switch expr options)))

(define (switch-to-cond switch-expr)
  (cons 'cond
        (map 
          (lambda (option)(cons (cons 'equal? (cons(car(cdr switch-expr))(cons (car option)nil)))(cdr option)))
          (car (cdr (cdr switch-expr)))
        )
  )
)
