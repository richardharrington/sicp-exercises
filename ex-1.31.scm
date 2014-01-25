;Exercise 1.31  

;(define (sum term a next b)
;  (if (> a b)
;      0
;      (+ (term a)
;         (sum term (next a) next b))))

;a. The sum procedure is only the simplest of a vast number of similar 
;abstractions that can be captured as higher-order procedures. Write an analogous 
;procedure called product that returns the product of the values of a function 
;at points over a given range. Show how to define factorial in terms of product. 
;Also use product to compute approximations to using the formula
;
;π/4 = (2 * 4 * 4 * 6 * 6 * 8...) / (3 * 3 * 5 * 5 * 7 * 7...)
;
;b. If your product procedure generates a recursive process, write one that 
;generates an iterative process. If it generates an iterative process, write 
;one that generates a recursive process.
;

;helpers

(define (identity x) x)
(define (inc x) (+ x 1))
(define (div x y) (floor (/ x y)))

;a.

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))

(define (pi-approx n)
  (define (top-term n)
    (+ (* (div n 2) 2) 2))
  (define (bottom-term n)
    (+ (* (div (inc n) 2) 2) 1))
  (* (/ (product top-term 1 inc n)
        (product bottom-term 1 inc n))
     4.0))

;b.

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))
  