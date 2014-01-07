; Exercise 1.8.

; Newton's method for cube roots is based on the fact that if 
; y is an approximation to the cube root of x, then a better 
; approximation is given by the value

; ((x/y^2) + 2y) / 3

; Use this formula to implement a cube-root procedure analogous 
; to the square-root procedure. (In section 1.3.4 we will see 
; how to implement Newton's method in general as an abstraction 
; of these square-root and cube-root procedures.)

(define (cube-root-iter x previous-guess guess)
  (if (good-enough? previous-guess guess)
      guess
      (cube-root-iter x
                      guess 
                      (improve guess x))))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* guess 2)) 3))

(define (square x)
  (* x x))

(define (good-enough? previous-guess guess)
  (< (/ (abs (- previous-guess guess)) guess) 
     0.001))

(define (cube-root x)
  (cube-root-iter x 1.0 (improve 1.0 x)))