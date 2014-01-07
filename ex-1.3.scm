; Exercise 1.3

; Define a procedure that takes three numbers as arguments 
; and returns the sum of the squares of the two larger numbers.

(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (min-of-two x y)
  (if (< x y) x y))

(define (min-of-three x y z)
  (min x (min y z)))

(define (f x y z)
  (cond
    ((= (min-of-three x y z) x) (sum-of-squares y z))
    ((= (min-of-two y z) y)     (sum-of-squares x z))
    (else                       (sum-of-squares x y))))
