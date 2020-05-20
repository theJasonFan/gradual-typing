#lang gtlc/intermediate
;#lang gtlc



;;; Hmmm this shouldn't typecheck... should it?
(letrec [(one <dyn> (λ [num -> num] (n) (if (eq? n 0) 0 (one "string"))))]
  (one 0))
 
;(letrec
;    [(tri ;<[dyn -> num]>
;         (λ (n) (if (eq? n 0)
;                    0
;                    (+ n (tri (- n 1))))))]
;  (tri 10))
;(λ (n) n)
;(<dyn> (λ [num -> num] (n) (+ n 1)) 1)

;<num> 1

;;#|
;;(castexp
;  (funT (inTs s0 .. sn) (outT sout))
;  (lambda (funT (inTs t0 ... tn) (outT tout)) (operands n) body)
;==> 
;(closure
; (lambda (z0 ... zn)
;   [s0 ... sn -> sout]
;   (cast sout
;   (apply (lambda (n) body)
;   ((cast t0 z0) ... (cast tn zn))))
; env)
