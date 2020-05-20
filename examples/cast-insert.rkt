#lang gtlc/cast-insert-only

(define (one n)
  (λ [num -> num] (n) 1))

((one 1) 1)