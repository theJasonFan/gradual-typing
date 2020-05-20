#lang racket
;; Environments map variables to mutable cells 
;; containing values.

(define-struct cell ([value #:mutable]))

; empty environment:
(define (env-empty)  (hash))

; initial environment, with bindings for primitives:
(define (env-initial)
  (env-extend* 
   (env-empty)
   '(+  -  /  *  <=  void  display  newline eq? and or)
   (map (lambda (s) (list 'primitive s))
   `(,+ ,- ,/ ,* ,<= ,void ,display ,newline ,eq? ,_and ,_or))))

; wrapping macros
(define (_and a b) (and a b))
(define (_or a b) (or a b))

; checks hash for value
(define (in-env? env var)
  (hash-has-key? env var))

; looks up a value:
(define (env-lookup env var)
  (cell-value (hash-ref env var)))

; sets a value in an environment:
(define (env-set! env var value)
  (set-cell-value! (hash-ref env var) value))

; extends an environment with several bindings:
(define (env-extend* env vars values)
  (match `(,vars ,values)
    [`((,v . ,vars) (,val . ,values))
     ; =>
     (env-extend* (hash-set env v (make-cell val)) vars values)]
    
    [`(() ())
     ; =>
     env]))

; mutates an environment with several assignments:
(define (env-set!* env vars values)
  (match `(,vars ,values)
    [`((,v . ,vars) (,val . ,values))
     ; =>
     (begin
       (env-set! env v val)
       (env-set!* env vars values))]
    
    [`(() ())
     ; =>
     (void)]))

(provide (all-defined-out))