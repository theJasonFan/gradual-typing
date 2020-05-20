#lang racket
(require "env.rkt")
(require "utils.rkt")

(provide (all-defined-out))

(define (typecheck e)
  (judge (initial-tenv) e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (initial-tenv)
  (env-extend* 
   (env-empty)
   '(+ - eq? and)
    (list
     (make-funT (list 'num 'num) 'num)
     (make-funT (list 'num 'num) 'num)
     (make-funT (list 'dyn 'dyn) 'bool)
     (make-funT (list 'bool 'bool) 'bool))))

(define (tenv-lookup G x)
  (if (in-env? G x)
      (env-lookup G x)
      (error "TypeError-UnboundVar" x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Consistency
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (consistent?* t0 t1)
  (if (empty? t0)
      (if (empty? t1) #t #f)
      (and (consistent? (first t0) (first t1))
           (consistent?* (rest t0) (rest t1)))))

;; Type Type -> Bool
(define (consistent? t0 t1)
  (or (eq? t0 t1)
      (eq? t0 'dyn)
      (eq? t1 'dyn)
      (CFun t0 t1))) ;;short circuiting avoids

;; Type Type -> Bool
;; s and t are not 'dyn.
(define (CFun s t)
  (and (and (funT? s) (funT? t)) ;; if not both functions, exit early
       (and (consistent?* (funT-inTs s) (funT-inTs t))
            (consistent? (funT-outT s) (funT-outT t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Convenience for currying judge
(define (judge-with G)
  (λ (e) (judge G e)))

;;TypeEnv Exp -> Type
(define (judge G e)
  (match e
    [(? number? x) 'num]
    [(? boolean? x) 'bool]
    [(? char? x) 'char]
    [(? string? x) 'string]
    [(? symbol? x) (tenv-lookup G x)]
    [`(let ,bs ,e0) (error "LET NOT IMPLEMENTED")]
    [`(lambda (operands ,vs ...) ,body) (GLam-dyn G vs body)]
    [`(lambda ,funT (operands ,vs ...)  ,body) (GLam G funT vs body)]
    [`(apply ,f . ,args) (GApp G f args)]
    [`(if ,ec ,et ,ef) (GIf G ec et ef)]
    [`(letrec (binds ,binds ...) ,e0) (GLetrec G binds e0)]))

(define (GLam-dyn G vs body)
  (let* ([inTs (map (λ _ 'dyn) vs)]
         [outT 'dyn]
         [funT (make-funT inTs outT)])
    (GLam G funT vs body)))

(define (GLam G funT vs body)
  (match funT
    [`(funT (inT ,inTs ...) (outT ,outT))
     ; =>
     (let* ([G-new (env-extend* G vs inTs)]
            [tbody (judge G-new body)])
       (if (consistent? tbody outT)
           funT
           (error "TypeError-ReturnTypeMismatch")))]
    ['dyn
     ;; We won't allow the programmer to define a function with type 'dyn
     (error "TypeError-function-type-dyn-not-allowed")]
    [_ (error "TypeError-DeclaredNotFun")]))

(define (GApp G e args)
  (let ([te (judge G e)]
        [targs (map (judge-with G) args)])
  (match te
    [(? dyn?) 'dyn]
    [(? funT?) (GApp2 e args te targs)]
    [_ (error "TypeError-ApplyNotFun")])))

(define (GApp2 e args te targs)
  (if (consistent?* (funT-inTs te) targs)
      (funT-outT te)
      (error "TypeError-TypeMismatch")))
      
(define (GIf G ec et ef)
  (let ([tc (judge G ec)]
        [tt (judge G et)]
        [tf (judge G ef)])
    (if (consistent? tc 'bool)
        (if (consistent? tt tf)
            (if (eq? tt tf)
                tt
                'dyn)
            (error "TypeError-InconsistentBranch" et ef))
        (error "TypeError-PremiseTypeMismatch" ec))))

(define (bind-lambda? b)
  (lambda? (third b)))


(define (clambda-funT e)
  (match e
    [(? lambda?) (lambda-funT e)]))

(define (GLetrec G binds e)
  (let*
      (;; evaluate the types
       [lambda-binds       (filter bind-lambda? binds)]
       [lambda-binds-vars    (map second lambda-binds)]
       [lambda-binds-vals (map third lambda-binds)]
       [lambda-binds-assume-funTs (map lambda-funT lambda-binds-vals)]
       [G-new (env-extend* G lambda-binds-vars lambda-binds-assume-funTs)]
       [lambda-binds-funTs (map (judge-with G-new) lambda-binds-vals)]
    )
  (env-set!* G-new lambda-binds-vars lambda-binds-funTs)
  (judge G-new e)))