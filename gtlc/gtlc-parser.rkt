#lang brag
;; NOTE on Cuts and splices to clean up parse tree
;; @ prefixes nodes that should be merged up the tree
;; / prefixes literals that need not be included in tree
;prog: define + exp
prog: define* exp

@exp: NUM | BOOL | STRING
   | ID
   | lambda
   | apply
   | let | letrec | if | define

define: /LPAR /DEFINE ID exp /RPAR
      | /LPAR /DEFINE /LPAR ID operands /RPAR exp /RPAR
      | /LPAR /DEFINE /LPAR ID operands /RPAR funT exp /RPAR

if: /LPAR /IF exp exp exp /RPAR
letrec: /LPAR /LETREC /LSB binds /RSB exp /RPAR
let: /LPAR /LET /LSB binds /RSB exp /RPAR
binds: bind +
bind: /LPAR ID exp /RPAR
apply: /LPAR exp exp* /RPAR
lambda: /LPAR /LAMBDA funT /LPAR operands /RPAR exp /RPAR
      | /LPAR /LAMBDA /LPAR operands /RPAR exp /RPAR
operands: ID*
type: funT | TYPE
funT: /LSB inT /ARROW outT /RSB
inT: @type*
outT: @type