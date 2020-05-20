#lang gtlc

(define (tri n)
  (if (eq? n 1)
      1
      (+ n (tri (- n 1)))))

(define (triT n) [num -> num]
  (if (eq? n 1)
      1
      (+ n (tri (- n 1)))))

(and (eq? (tri 10) (triT 10))
     (eq? (triT 10) 55))
