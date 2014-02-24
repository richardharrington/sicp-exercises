;Exercise 1.18. 
;
;Using the results of exercises 1.16 and 1.17, devise a procedure that generates 
;an iterative process for multiplying two integers in terms of adding, doubling,
;and halving and uses a logarithmic number of steps.

(define (double n)
  (* n 2))

(define (halve n)
  (/ n 2))

(define (fast-mult a b)
  (fast-mult-iter a b 0))

(define (fast-mult-iter a b sum)
  (cond 
    ((= b 0) sum)
    ((even? b) (fast-mult-iter (double a) (halve b) sum))
    (else (fast-mult-iter a (- b 1) (+ sum a)))))



