;Exercise 1.33.

;helpers

(define (identity x) x)
(define (inc x) (+ x 1))
(define (square x) (* x x))

(define (prime? n)
  (define (divides? a b)
    (= remainder b a) 0)
  (define (find-divisor n test-divisor)
    (cond
      ((> (square test-divisor) n) n)
      ((divides? test-divisor n) test-divisor)
      (else (find-divisor n (+ test-divisor 1)))))
  (define (smallest-divisor n)
    (find-divisor n 2))
  (= n (smallest-divisor n)))

(define (relative-prime? a b)
  (define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))
  (= (gcd a b) 1))

;filtered-accumulate

(define (filtered-accumulate filter combiner null-value term a next b)
  (cond
    ((> a b) null-value)
    ((filter a) (combiner (term a)
                          (filtered-accumulate filter combiner null-value term (next a) next b)))
    (else (filtered-accumulate filter combiner null-value term (next a) next b))))

;a.

(define (sum-of-squares-of-primes a b)
  (filtered-accumulate prime? + 0 square inc b))

;b.

(define (product-of-relative-primes n)
  (filtered-accumulate relative-prime? * 1 identity 1 inc (- n 1)))