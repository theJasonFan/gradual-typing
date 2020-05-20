#lang racket
(require "gtlc-lexer.rkt" brag/support rackunit)
(require "gtlc-parser.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TESTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ test
  (require rackunit)
  (check-equal? (lex "123") (list (token 'NUM 123)))
  (check-equal? (lex "lambda") (list (token 'LAMBDA)))
  (check-equal? (lex "lambda123") (list (token 'ID 'lambda123)))
  (check-equal? (lex "123lambda") (list (token 'ID '123lambda)))
  (check-equal? (lex "123lambdalex([12 3]) trues true lambda")
                (list
                 (token-struct 'ID '123lambdalex #f #f #f #f #f)
                 (token-struct 'LPAR #f #f #f #f #f #f)
                 (token-struct 'LSB #f #f #f #f #f #f)
                 (token-struct 'NUM 12 #f #f #f #f #f)
                 (token-struct 'WHITESPACE #f #f #f #f #f #t)
                 (token-struct 'NUM 3 #f #f #f #f #f)
                 (token-struct 'RSB #f #f #f #f #f #f)
                 (token-struct 'RPAR #f #f #f #f #f #f)
                 (token-struct 'WHITESPACE #f #f #f #f #f #t)
                 (token-struct 'ID 'trues #f #f #f #f #f)
                 (token-struct 'WHITESPACE #f #f #f #f #f #t)
                 (token-struct 'BOOL #t #f #f #f #f #f)
                 (token-struct 'WHITESPACE #f #f #f #f #f #t)
                 (token-struct 'LAMBDA #f #f #f #f #f #f)))
  (check-equal? (lex "<num> 1")
                (list
                 (token-struct 'LCAR #f #f #f #f #f #f)
                 (token-struct 'TYPE 'num #f #f #f #f #f)
                 (token-struct 'RCAR #f #f #f #f #f #f)
                 (token-struct 'WHITESPACE #f #f #f #f #f #t)
                 (token-struct 'NUM 1 #f #f #f #f #f)))
  )

(module+ test
  (require rackunit)
  (check-equal? (parse-to-datum (lex "(lambda [string [-> num] -> [-> num]] () (+ 1 true \"str\"))"))
                '(prog
                  (lambda
                      (funT (inT string (funT (inT) (outT num)))
                            (outT (funT (inT) (outT num))))
                    (operands)
                    (apply + 1 #t "str"))))
  (check-equal? (parse-to-datum (lex "(lambda [-> [-> num]] () (+ 1 true \"str\"))"))
                '(prog
                  (lambda
                      (funT (inT) (outT (funT (inT) (outT num))))
                    (operands)
                    (apply + 1 #t "str"))))
  )