#lang br/quicklang
(require syntax/strip-context)
(require "intermediate-parser.rkt")
(require "gtlc-tokenizer.rkt")
 
(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port path)))
  (strip-bindings
   #`(module basic-parser-mod gtlc/gtlc-parse-only
       #,parse-tree)))
(module+ reader (provide read-syntax))

(define-macro (parser-only-mb PARSE-TREE)
  #'(#%module-begin
     'PARSE-TREE))
(provide (rename-out [parser-only-mb #%module-begin]))