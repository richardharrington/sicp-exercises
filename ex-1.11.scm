; Exercise 1.11.  A function f is defined by the rule that 
; f(n) = n if n<3 and f(n) = f(n-1) + 2f(n-2) + 3f(n-3) if n>=3. 
; Write a procedure that computes f by means of a recursive process. 
; Write a procedure that computes f by means of an iterative process.

(define (recurs-f n)
  (cond ((< n 3) n)
        (else (+ (recurs-f (- n 1))
                 (* 2 (recurs-f (- n 2)))
                 (* 3 (recurs-f (- n 3)))))))

(define (iter-f n)
  (define (helper a b c counter) 
    (cond ((= counter 0) c)
          (else (helper (+ a (* b 2) (* c 3)) a b (- counter 1))))) 
  (helper 2 1 0 n))

