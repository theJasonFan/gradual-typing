#lang racket
(require "cast-insert.rkt" brag/support rackunit)
(require "desugar.rkt" "gtlc-lexer.rkt")
(require "gtlc-parser.rkt")

(define (=> p)
  (CInsert-init (last (desugar (parse-to-datum (lex p))))))

(define (=>> p)
  (CInsert-init (desugar (parse-to-datum (lex p)))))

(check-equal? (=> "1") (TExp 1 'num))
(check-equal? (=> "true") (TExp #t 'bool))
(check-equal? (=> "\"foo\"") (TExp "foo" 'string))

(check-equal? (=> "(λ (x) x)")
              (TExp '(lambda
                       (funT (inT dyn) (outT dyn))
                       (operands x)
                       x)
                    '(funT (inT dyn) (outT dyn))))

; NOTE: should this typecheck???
; are we wrong for specifying return types when they can be inferred?
; Nothing in rules checks for this...
(check-equal? (=> "(λ [dyn -> num] (x) x)")
              (TExp '(lambda
                       (funT (inT dyn) (outT num))
                       (operands x)
                       x)
                   '(funT (inT dyn) (outT num))))

(check-exn exn:fail? (λ _ (=> "(λ [dyn -> num] (x) true)")))

(check-equal? (=> "((λ [num -> dyn] (x) true) 1)")
              (TExp
                '(apply
                  (lambda (funT (inT num) (outT dyn))
                    (operands x)
                    #t)
                  1)
                'dyn))

(check-equal? (=> "((λ [dyn -> dyn] (x) true) 1)")
              (TExp
                '(apply
                  (lambda (funT (inT dyn) (outT dyn))
                    (operands x)
                    #t)
                  (castexp dyn 1))
                'dyn))

