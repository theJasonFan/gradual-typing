#lang racket
(provide (all-defined-out))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Types and useful predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dyn? t) (eq? t 'dyn))

(define (make-funT inTs outT) `(funT (inT . ,inTs) (outT ,outT)))

;;
(define (funT? t)
  (match t
    [`(funT (inT ,inTs ...) (outT ,outT)) #t]
    [_ #f]))

(define (funT-inTs t)
  (match t
    [`(funT (inT ,inTs ...) (outT ,outT)) inTs]))
  
(define (funT-outT t)
  (match t
    [`(funT (inT ,inTs ...) (outT ,outT)) outT]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Types and useful predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lambda? e)
  (eq? (first e) 'lambda))

(define (lambda-operands e)
  (match e
    [`(lambda ,funT (operands . ,vs) ,body) vs]
    [`(lambda (operands . ,vs) ,body) vs]))

(define (lambda-funT e)
  (match e
    [`(lambda ,funT ,vs ,body) funT]
    [`(lambda (operands ,vs ...) ,body)
     (make-funT (map (λ _ 'dyn) vs) 'dyn)]))

(define (lambda-insert-dyn-funT e)
  (match e
    [`(lambda (operands . ,vs) ,body)
     `(lambda
         ,(make-funT (map (λ (_) 'dyn) vs)
                     'dyn)
       (operands . ,vs)
       ,body)]))

(define (insert-casts* inTs zs)
  (if (empty? inTs)
      '()
      (cons `(castexp ,(first inTs) ,(first zs))
            (insert-casts* (rest inTs) (rest zs)))))