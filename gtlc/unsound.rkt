#lang br/quicklang
(require syntax/strip-context)
(require "gtlc-parser.rkt")
(require "gtlc-tokenizer.rkt")
(require "env.rkt")
(require "desugar.rkt")
 
(define (read-syntax path port)
  (define parse-tree (desugar (syntax->datum (parse path (make-tokenizer port path)))))
  (define _ (typecheck parse-tree))
  (define result (eval-init parse-tree))
  (strip-bindings
   #`(module basic-parser-mod gtlc/gtlc-parse-only
       #,result)))
(module+ reader (provide read-syntax))

(define-macro (parser-only-mb PARSE-TREE)
  #'(#%module-begin
     'ignoreme))
(provide (rename-out [parser-only-mb #%module-begin]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; EVAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (eval-init e)
  (eval e (env-initial)))

(define (eval e env)
  ;(display e) (display "\n")
  (match e
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
    [`(let    (binds ,binds ...) ,eb) (eval-let binds eb env)]
    
    [`(lambda ,funT ,vs  ,body) `(closure (lambda ,vs ,body) ,env)] ;; ignore type signature
    [`(lambda ,vs ,body)       `(closure ,e ,env)]
    [`(apply ,f . ,args) (apply-proc
                           (eval f env)
                           (map (eval-with env) args))]))
;)

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
(define (eval-let bindings body env)
  (let* ((vars (map second bindings))
         (exps (map third bindings))
         (vals (map (eval-with env) exps))
         (env* (env-extend* env vars vals)))
    (eval body env*)))

; applies a procedure to arguments:
(define (apply-proc f values) 
  (match f
    [`(closure (lambda (operands ,vs ...) ,body) ,env)
     ; =>
     (eval body (env-extend* env vs values))]
    [`(closure (lambda ,funT (operands ,vs ...) ,body) ,env)
     ; =>
     (eval body (env-extend* env vs values))]
    [`(primitive ,p)
     ; =>
     (apply p values)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Typecheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(require "gtlc-typecheck")
(define (typecheck x) x)