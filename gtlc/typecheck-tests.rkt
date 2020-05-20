#lang racket
(require "typecheck.rkt" brag/support rackunit)
(require "desugar.rkt" "gtlc-lexer.rkt")
(require "gtlc-parser.rkt")
(require "utils.rkt")

(define (type p)
  (typecheck (desugar (parse-to-datum (lex p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TESTS: Consistency
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-equal? (make-funT '(a) 'b) '(funT (inT a) (outT b)))
(check-equal? (make-funT '(a b) 'c) '(funT (inT a b) (outT c)))

(check-true  (consistent? 'a 'a))
(check-false (consistent? 'a 'b))
(check-false (consistent? 'b 'a))
(check-true  (consistent? 'a 'dyn))
(check-true  (consistent? 'dyn 'a))
(check-true  (consistent? 'dyn (make-funT '(a) 'b)))
(check-true  (consistent? (make-funT '(a) 'b) 'dyn))
(check-false (consistent? (make-funT '(a) 'b) (make-funT '(dyn) 'a)))
(check-true  (consistent? (make-funT '(dyn) 'dyn) (make-funT '(a) 'b)))
(check-false (consistent? 'a (make-funT '(a) 'b)))
(check-true  (consistent? (make-funT '(dyn) 'dyn) (make-funT '(a) (make-funT '(a) 'b))))
(check-false (consistent? (make-funT '(a) 'a) (make-funT '(a) (make-funT '(a) 'b))))
(check-true  (consistent? (make-funT '(a) 'dyn) (make-funT '(a) (make-funT '(a) 'b))))
(check-true  (consistent? (make-funT '(dyn) 'b) (make-funT `(,(make-funT '(a) 'a)) 'b)))
(check-false (consistent? (make-funT '(dyn) 'b) (make-funT '(a) (make-funT '(a) 'b))))

(check-exn exn:fail? (λ () (type "a")))
(check-equal? (type "1") 'num)
(check-equal? (type "\"str\"") 'string)
(check-equal? (type "true") 'bool)
(check-equal? (type "+") (make-funT (list 'num 'num) 'num))

(check-equal? (type "(+ 1 2)") 'num)
(check-equal? (type "(λ [num -> num] (x) 1)") (make-funT '(num) 'num))
(check-equal? (type "((λ [num -> num] (x) 1) 1)") 'num)
(check-exn exn:fail? (λ () (type "((λ [num -> num] (x) 1) \"str\")")))
(check-equal? (type "((λ [dyn -> num] (x) 1) 1)") 'num)

;; always take programmers type
;; since they can write something like
;; (λ [ ... -> num] (...) _CONST) where _CONST has type dyn.
(check-equal? (type "((λ [num -> dyn] (x) 1) 1)") 'dyn) 
;;

(check-equal? (type "(λ (x) 1)") (make-funT '(dyn) 'dyn))
(check-equal? (type "(λ (x y) 1)") (make-funT '(dyn dyn) 'dyn))
(check-equal? (type "((λ (x) 1) 1)") 'dyn)
(check-equal? (type "((λ (x) 1) \"str\")") 'dyn)
(check-equal? (type "(λ (x) x)") (make-funT '(dyn) 'dyn))

;; TODO: should this case be 'num?
;; Problem: no notion of precision here, judgement rule only checks if the inputs
;; are consistent with declared types of the operands. The type of the *result*
;; cannot be evaluated.
(check-equal? (type "((λ (x) x) 1)") 'dyn)
(check-equal? (type "(if true 1 1)") 'num)

;; Note: punt on this case where branches are of different types.
;; Again, no notion of precision here, so even if types are consistent,
;; we don't know which to return.
;(check-equal? (type "(if true \"s\" 1)") 'dyn)

(check-equal? (type "(define (id x) x) (id 1)") 'dyn)
(check-equal? (type "(define (tri n) [num -> num] (if (eq? n 0) 0 (+ n (tri (- n 1))))) (tri 0)")
              'num)
