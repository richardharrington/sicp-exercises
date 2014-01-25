; Exercise 1.29

; Write a function to approximate an integral
; using Simpson's Rule (look it up; too hard to describe it here
; without using LaTeX or some such thing).

(define (cube x) (* x x x))
(define (inc x) (+ x 1))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (y k)
    (f (+ a (* k h))))
  (define (simpson-term k)
    (* (y k) 
       (+ (* (remainder k 2) 2) 2)))
  (* (+ (sum simpson-term 1 inc n) 
        (y 0))
     (/ h 3.0) ))
  
    