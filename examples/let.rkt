#lang gtlc/typecheck-only
(letrec
  [(f (λ [dyn -> num] (x)
      (+ x 5)))]
  (f 20))