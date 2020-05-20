#lang br/quicklang
(require syntax/strip-context)
(require "intermediate-parser.rkt")
(require "gtlc-tokenizer.rkt")
(require "desugar.rkt")
(require "typecheck.rkt")
(require "eval-intermediate.rkt")
 
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