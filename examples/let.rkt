#lang gtlc/cast-insert-only

(letrec
  [(f (λ [dyn -> num] (x)
      (+ <num> x <num> 5)))]
  (f <dyn> 20))