(define (ascending? s) 
    (cond 
        ((or (null? s)(null? (cdr s))) #t)
        ((> (car s)(car (cdr s))) #f)
        (else (ascending? (cdr s)))
    )
)

(define (my-filter pred s) 
    (cond 
        ((null? s) nil)
        ((pred (car s)) (cons (car s) (my-filter pred (cdr s))))
        (else (my-filter pred (cdr s)))
    )
)

(define (interleave lst1 lst2) 
    (cond 
        ((and (null? lst1) (null? lst2)) nil )
        ((null? lst1) (cons (car lst2)(interleave nil (cdr lst2))))
        ((null? lst2) (cons (car lst1)(interleave (cdr lst1) nil)))
        (else (cons (car lst1)(cons (car lst2) (interleave (cdr lst1) (cdr lst2)))))
    )
)

(define (no-repeats s) 
    (define (ff x y)
        (cond 
            ((null? x) #t)
            ((= (car x) y) #f)
            (else (ff (cdr x) y))
        )
    )
    (define (gg xx yy)
        (cond 
            ((null? xx) yy)
            ((ff yy (car xx)) (gg (cdr xx)(append yy (cons (car xx) nil ))))
            (else (gg (cdr xx) yy ))
        )
    )
    (gg s nil )
)
