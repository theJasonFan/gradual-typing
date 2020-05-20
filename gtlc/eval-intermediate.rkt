#lang racket
(require "env.rkt")
(require "utils.rkt")
(require "typecheck.rkt")

(provide (all-defined-out))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; EVAL cast language
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (eval-init e)
  (eval e (env-initial)))

(define (eval e env)
  (match e
    [`(castexp ,cast ,e) (eval-cast cast e env)]
    [(? number? x) x]
    [(? string? x) x]
    [`(if ,ec ,et ,ef)    (if (eval ec env)
                              (eval et env)
                              (eval ef env))]
    [(? symbol? x)          (env-lookup env x)]
    
    [(? boolean? x)         x]
    [`(if ,ec ,et ,ef)    (if (eval ec env)
                              (eval et env)
                              (eval ef env))]
    [`(letrec (binds ,binds ...) ,eb) (eval-letrec binds eb env)]
    ;[`(let    (binds ,binds ...) ,eb) (eval-let binds eb env)]
    [`(lambda ,funT ,vs ,body) `(closure ,e ,env)] 
    [`(lambda ,vs ,body)       `(closure ,(lambda-insert-dyn-funT e) ,env)]
    [`(apply ,f . ,args) (apply-proc
                           (eval f env)
                           (map (eval-with env) args))]
    [`(apply (castexp ,funT ,lambda)) 'exit]))

;; Ground types for values
(define (type v)
  (match v
    [(? number?) 'num]
    [(? boolean?) 'bool]
    [(? string?) 'string]))

(define (eval-cast castT e env)
  (match castT
    [(? funT?) (ECstF castT e env)]
    ['dyn (ECstU e env)]
    [(? symbol?) (ECstG castT e env)]))

(define (ECstF CfunT e env)
     (let* ([closure (eval e env)]
            [λexp (second closure)]
            [funT (lambda-funT λexp)])
       (if (consistent? CfunT funT)
           (ECstClosure CfunT closure)
           (error "TypeError-funT-mismatch" CfunT funT))))

(define (ECstClosure CfunT closure)
  (match closure
    [`(closure ,lambda ,env)
     ; =>
     (let* [(funT (second lambda))
            (inTs (funT-inTs funT))
            (coutT (funT-outT CfunT))
            (zs (map (λ (_) (gensym)) inTs))
            (castvars (insert-casts* inTs zs))
            (body-new `(castexp ,coutT (apply ,lambda . ,castvars)))]
     `(closure
       (lambda ,CfunT (operands . ,zs) ,body-new) ,env))]))

(define (ECstG t e env)
  (let* ([v (eval e env)]
         [γ (type v)])
    (if (consistent? t γ)
        v
        (error "CastError" t γ))))

(define (ECstU e env)
  (eval e env))

; a handy wrapper for Currying eval:
(define (eval-with env)
  (lambda (exp) (eval exp env)))

; eval for letrec:
(define (eval-letrec bindings body env)
  (let* ((vars (map second bindings))
         (exps (map third bindings))
         (fs   (map (lambda _ #f) bindings))
         (env* (env-extend* env vars fs))
         (vals (map (eval-with env*) exps)))
    (env-set!* env* vars vals)
    (eval body env*)))

; eval for let:
;(define (eval-let bindings body env)
;  (let* ((vars (map second bindings))
;         (exps (map third bindings))
;         (vals (map (eval-with env) exps))
;         (env* (env-extend* env vars vals)))
;    (eval body env*)))

; applies a procedure to arguments:
(define (apply-proc f values)
  (match f
   [`(closure (lambda (funT ,ts ...) (operands ,vs ...) ,body) ,env)
     ; =>
     (eval body (env-extend* env vs values))]
    
   [`(primitive ,p)
     ; =>
     (apply p values)]))