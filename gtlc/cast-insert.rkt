#lang racket
(require "env.rkt" "typecheck.rkt")
(require "utils.rkt")
(provide (all-defined-out))

;; Typed expression
(struct TExp (exp type) #:transparent)

(define (CInsert-with G)
  (λ (e) (CInsert G e)))

(define (CInsert-init e)
  (CInsert (initial-tenv) e))

;; After cast insertion, no TExp should be inside the topmost TExp
; TEnv Exp -> TExp
(define (CInsert G e)
  (match e
    [(? number?)  (TExp e 'num)]
    [(? boolean?) (TExp e 'bool)]
    [(? char?)    (TExp e 'char)]
    [(? string?)  (TExp e 'string)]
    [(? symbol?)  (TExp e (tenv-lookup G e))]
    [`(lambda (operands ,vs ...) ,body) (CInsert G (lambda-insert-dyn-funT e))]
    [`(lambda ,funT (operands ,vs ...)  ,body) (CLam G funT vs body)]
    [`(apply ,f . ,args) (CApp G f args)]
    [`(if ,ec ,et ,ef)  (CIf G ec et ef)]
    [`(letrec (binds ,binds ...) ,x) (CLetrec G binds x)]))

(define (CIf G ec et ef)
  (let* ([ec-texp (CInsert G ec)]
         [et-texp (CInsert G et)]
         [ef-texp (CInsert G ef)])
    (if (and (eq? (TExp-type ec-texp) 'bool)
             (consistent? (TExp-type et-texp) (TExp-type ef-texp)))
        (if (eq? (TExp-type et-texp) (TExp-type ef-texp))
            (TExp `(if ,(TExp-exp ec-texp) ,(TExp-exp et-texp) ,(TExp-exp ef-texp))
                  (TExp-type et-texp))
            (error "InsertError-IfUnequalBranchTypes"))
        (error "InsertError-IfCondNotBoolOrInconsistentBranches"))))

(define (CLam G funT vs body)
  (let* ([inTs (funT-inTs funT)]
         [G-new (env-extend* G vs inTs)]
         [texp (CInsert G-new body)])
    (if (consistent? (TExp-type texp) (funT-outT funT))
        (TExp `(lambda
                 ,funT
                 (operands . ,vs)
                 ,(TExp-exp texp))
              funT)
        (error "InsertError-ReturnTypeMismatch")))) ;;should never get here

(define (CApp G f args)
  (let* ([λtexp (CInsert G f)]
         [args-texps (map (CInsert-with G) args)]
         [args-exps (map TExp-exp args-texps)]
         [argTs (map TExp-type args-texps)])
    (if (dyn? (TExp-type λtexp))
        (TExp
         `(apply
           (castexp ,(make-funT argTs 'dyn)
                    ,(TExp-exp λtexp)) .
           ,args-exps)
         'dyn)
        ;==> CAPP2 and CAPP 3
        (let*
            ([funT (TExp-type λtexp)]
             [inTs (funT-inTs funT)]
             [outT (funT-outT funT)])
          (if (equal? argTs inTs) ;; note: eq? != equal?
              ;===> CAPP3
              (TExp
               `(apply
                 ,(TExp-exp λtexp) .
                 ,args-exps)
               outT)
              ;===> CAPP2
              (if (consistent?* argTs inTs)
                  (TExp
                   `(apply
                     ,(TExp-exp λtexp) .
                     ,(insert-casts* inTs args-exps))
                   outT)
                  (error "InsertError-TypeMismatch"))))))) ;;should never get here

(define (bind-lambda? b)
  (match (third b)
    [(? lambda?) #t]
    [(? castexp? c) (casted-lambda? c)]))

(define (make-binds vars vals)
  (if (empty? vars)
      '()
      (cons `(bind ,(first vars) ,(first vals))
            (make-binds (rest vars) (rest vals)))))

(define (CLetrec G binds e)
  (let*
      (;; evaluate the types
       [lambda-binds       (filter bind-lambda? binds)]
       [lambda-binds-vars    (map second lambda-binds)]
       [lambda-binds-vals (map third lambda-binds)]
       [lambda-binds-assume-funTs (map lambda-funT lambda-binds-vals)]
       [G-new (env-extend* G lambda-binds-vars lambda-binds-assume-funTs)]
       [lambda-binds-texps (map (CInsert-with G-new) lambda-binds-vals)]
       [lambda-binds-funTs (map TExp-type lambda-binds-texps)]
       [lambda-binds-exps  (map TExp-exp lambda-binds-texps)]
       [_ (env-set!* G-new lambda-binds-vars lambda-binds-funTs)]
       [new-binds `(binds . ,(make-binds lambda-binds-vars lambda-binds-exps))]
       [letrec-texp (CInsert G-new e)])
  (TExp
   `(letrec
        ,new-binds
        ,(TExp-exp letrec-texp))
   (TExp-type letrec-texp))))
