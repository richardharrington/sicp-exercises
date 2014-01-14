; Exercise 1.12.  Write a procedure that computes elements of 
; Pascal's triangle by means of a recursive process.

(define (pascal row column)
  (cond 
    ((or (= column 1) (= column row)) 1)
    (else (+ (pascal (- row 1) (- column 1))
             (pascal (- row 1) column)))))
