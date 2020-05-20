#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Desugar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (desugar prog)
  ;(display prog)
  (match prog
    [`(prog ,defines ... ,exp)
     `(letrec ,(cons 'binds
                     (map desugar-define defines))
        ,exp)]))
    ;[`(prog ,exp) exp])

(define (desugar-define define)
  (match define
    [`(define ,id ,e) `(bind ,id ,e)]
    [`(define ,id ,operands ,e) `(bind ,id (lambda ,operands ,e))]
    [`(define ,id ,operands ,funT ,e) `(bind ,id (lambda  ,funT ,operands ,e))]))

(provide (all-defined-out))