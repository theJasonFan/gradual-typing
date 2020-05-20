#lang gtlc/typecheck-only
;#lang gtlc/gtlc-parse-only
(define (even? n) [num -> bool]
  (if (eq? n 0)
      true
      (odd? (- n 1))))

(define (odd? n)
  (if (eq? n 1)
      true
      (even? (- n 1))))

(even? 4)