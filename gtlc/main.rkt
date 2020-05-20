#lang br/quicklang
(require syntax/strip-context)
(require "gtlc-parser.rkt")
(require "gtlc-tokenizer.rkt")
(require "env.rkt")
(require "desugar.rkt")
(require "cast-insert.rkt")
(require "eval-intermediate.rkt")

 
(define (read-syntax path port)
  (define texp (CInsert-init (desugar (syntax->datum (parse path (make-tokenizer port path))))))
  (define result (eval-init (TExp-exp texp)))
  (strip-bindings
   #`(module basic-parser-mod gtlc/gtlc-parse-only
       #,result)))
(module+ reader (provide read-syntax))

(define-macro (parser-only-mb PARSE-TREE)
  #'(#%module-begin
     'ignoreme))
(provide (rename-out [parser-only-mb #%module-begin]))