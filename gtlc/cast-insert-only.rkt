#lang br/quicklang
(require syntax/strip-context)
(require "gtlc-parser.rkt")
(require "gtlc-tokenizer.rkt")
(require "env.rkt")
(require "desugar.rkt")
(require "cast-insert.rkt")
 
(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port path)))
  (define result (CInsert-init (desugar (syntax->datum parse-tree))))
  (strip-bindings
   #`(module basic-parser-mod gtlc/gtlc-parse-only
       #,result)))
(module+ reader (provide read-syntax))

(define-macro (parser-only-mb PARSE-TREE)
  #'(#%module-begin
     'ignoreme))
(provide (rename-out [parser-only-mb #%module-begin]))


