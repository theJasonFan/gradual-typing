#lang gtlc

;; has type [dyn -> dyn]
(define (one n)
  (λ [num -> num] (n) 1))

;; (one 1) : dyn
((one 1) 1)