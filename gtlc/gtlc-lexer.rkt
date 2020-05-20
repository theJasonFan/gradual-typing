#lang racket
(require brag/support)
;(require "gtlc-parser.rkt")
;(require parser-tools/lex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LEXER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Not using "alphabetic", "numeric", etc. because not sure which chars
;; exactly they include
(define-lex-abbrev digit (char-set "0123456789"))
(define-lex-abbrev digits (:+ digit))
(define-lex-abbrev float (:or (:seq (:? digits) "." digits) ; 123.456 or .456
                              (:seq digits ".")))           ; 123.

(define-lex-abbrev letter (:or
                           (char-range #\A #\Z)
                           (char-range #\a #\z)))
;;todo: add-lambdas
;;todo: should just be the *not* of delimiters.
(define-lex-abbrev special-initial (char-set "!$%&*/:=?^_-+.λ"))

(define-lex-abbrev initial (:or
                            digit
                            letter
                            special-initial))
(define-lex-abbrev delimiter (:or (char-set "()[]<>") whitespace))

;; lexer matches the "longest" match in the list, so if a string is prefixed b
;; {lambda, float} it will match until whitespace.
(define gtlc-lexer
  (lexer
   ;; keywords
   ["define" (token 'DEFINE)]
   ["λ" (token 'LAMBDA)]
   ["lambda" (token 'LAMBDA)]
   ["->" (token 'ARROW)]
   ["let" (token 'LET)]
   ["letrec" (token 'LETREC)]
   ["if" (token 'IF)]
   
   ;; types
   ;; note: type keywords are reserved...
   ["dyn" (token 'TYPE 'dyn)]
   ["bool" (token 'TYPE 'bool)]
   ["num" (token 'TYPE 'num)]
   ["string" (token 'TYPE 'string)]

   ;; nums
   [(:or digits float)  (token 'NUM (string->number lexeme))]

   ;; strings
   [(:or (from/to "\"" "\"") (from/to "'" "'"))
    (token 'STRING
           (substring lexeme
                      1 (sub1 (string-length lexeme))))]

   ;; bools
   ["false" (token 'BOOL #f)] ;python style bools. problem here is that lexing #fabc is tricky
   ["true" (token 'BOOL #t)]  ; must come before ID matching case

   ;; identifiers
   [(from/stop-before initial delimiter) (token 'ID (string->symbol lexeme))]
   [(from/stop-before ";" "\n") (token 'COMMENT lexeme #:skip? true)]
   ;; delimiters
   ["<" (token 'LCAR)]
   [">" (token 'RCAR)]
   
   ["(" (token 'LPAR)]
   [")" (token 'RPAR)]
   ["[" (token 'LSB)]
   ["]" (token 'RSB)]
   [whitespace (token 'WHITESPACE #:skip? #t)]))

(define (lex str)
  (apply-port-proc gtlc-lexer str))

(provide lex)
(provide gtlc-lexer)

;(parse-to-datum (lex "(lambda (x y) [-> dyn dyn] (+ (+ 1 true) \"foo\"))"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Notes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Final pipeline example
;(eval (typecheck (parse (gtlc-lexer (open-input-string "(+ 1 2)")))))

#|
(define (f args) (-> sig) body)
;=> (letrec (f (λ ...))
(λ (args) (-> sig) body)|#