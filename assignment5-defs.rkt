#lang plai

; Assignment 5
; Written by Liam Murphy


(define-type F1WAE
  [num (n number?)]
  [id (name symbol?)]
  [add (lhs F1WAE?) (rhs F1WAE?)]
  [sub (lhs F1WAE?) (rhs F1WAE?)]
  [mul (lhs F1WAE?) (rhs F1WAE?)]
  [div (lhs F1WAE?) (rhs F1WAE?)]
  [mod (lhs F1WAE?) (rhs F1WAE?)]
  [minus (expr F1WAE?)]
  [with (name symbol?) (named-expr F1WAE?) (body F1WAE?)]
  [if< (expr1 F1WAE?) (expr2 F1WAE?) (val1 F1WAE?) (val2 F1WAE?)]
  [app (fun-name symbol?) (arg F1WAE?)])

(define-type FunDef
  [fundef (fun-name symbol?)
          (arg-name symbol?)
          (body F1WAE?)])

;; lookup-fundef: symbol listof(FunDef) --> FunDef
(define (lookup-fundef name defs)
  (cond [(null? defs) (error name "definition not found")]
        [(symbol=? name (fundef-fun-name (car defs))) ;; using the accessor function for fun-name
             (car defs)]
        [else (lookup-fundef name (cdr defs))]
     ))

;; subst: F1WAE symbol F1WAE --> F1WAE
(define (subst expr sub-id val)
  (type-case F1WAE expr
    [num (n) expr]
    [add (l r) (add (subst l sub-id val)
                    (subst r sub-id val))]
    [sub (l r) (sub (subst l sub-id val)
                    (subst r sub-id val))]
    [mul (l r) (mul (subst l sub-id val)
                    (subst r sub-id val))]
    [div (l r) (div (subst l sub-id val)
                    (subst r sub-id val))]
    [mod (l r) (mod (subst l sub-id val)
                    (subst r sub-id val))]
    [minus (n) (minus (subst n sub-id val))]
    [with (bound-id named-expr bound-body)
          (if (symbol=? bound-id sub-id)
              (with bound-id
                    (subst named-expr sub-id val)
                    bound-body)
              (with bound-id
                    (subst named-expr sub-id val)
                    (subst bound-body sub-id val)))]
    [app (fun-name arg-expr)
         (app fun-name (subst arg-expr sub-id val))]
    [id (v) (if (symbol=? v sub-id) val expr)]
    [if< (expr1 expr2 val1 val2) (if< (subst expr1 sub-id val)
                                      (subst expr2 sub-id val)
                                      (subst val1 sub-id val)
                                      (subst val2 sub-id val))]
))


;; interp: F1WAE listof(FunDef) --> number
(define (interp expr fun-defs)
  (type-case F1WAE expr
    [num (n) n]
    [add (l r) (+ (interp l fun-defs) (interp r fun-defs))]
    [sub (l r) (- (interp l fun-defs) (interp r fun-defs))]
    [mul (l r) (* (interp l fun-defs) (interp r fun-defs))]
    [div (l r) (quotient (interp l fun-defs) (interp r fun-defs))]
    [mod (l r) (remainder (interp l fun-defs) (interp r fun-defs))]
    [minus (n) (* (interp n fun-defs) -1)]
    [with (bound-id named-expr bound-body)
          (interp (subst bound-body
                         bound-id
                         (num (interp named-expr fun-defs)))
                  fun-defs)]
    [id (v) (error 'interp "free identifier: ~a" v)]
    [app (fun-name arg-expr)
         ;; In the next line, "let" is a short-cut for
         ;; "(local (define ..." which is what K. uses.
         ;; It's the Scheme version of our "with" expression.
         (let ([the-fun-def (lookup-fundef fun-name fun-defs)])
           (interp (subst (fundef-body the-fun-def)
                          (fundef-arg-name the-fun-def)
                          (num (interp arg-expr fun-defs)))
                   fun-defs))]
    [if< (expr1 expr2 val1 val2) (if (< (interp expr1 fun-defs) (interp expr2 fun-defs)) (interp val1 fun-defs) (interp val2 fun-defs))]
  ))

;; parse: s-expr -->F1WAE
(define (parse sexp)
  (cond [(number? sexp) (num sexp)]
        [(symbol? sexp) (id sexp)]
        [(eq? (first sexp) '+) (add (parse (second sexp))
                                    (parse (third sexp)))]
        [(eq? (first sexp) '-) (cond [(= (length sexp) 3) (sub (parse (second sexp))
                                                               (parse (third sexp)))]
                                     [(= (length sexp) 2) (minus (parse (second sexp)))])]
        [(eq? (first sexp) '*) (mul (parse (second sexp))
                                    (parse (third sexp)))]
        [(eq? (first sexp) '/) (div (parse (second sexp))
                                    (parse (third sexp)))]
        [(eq? (first sexp) '%) (mod (parse (second sexp))
                                    (parse (third sexp)))]
        [(eq? (first sexp) 'if<) (if< (parse (second sexp))
                                      (parse (third sexp))
                                      (parse (fourth sexp))
                                      (parse (fifth sexp)))]
        [(eq? (first sexp) 'with) (with (first (second sexp))
                                    (parse (second (second sexp)))
                                    (parse (third sexp)))]
        [(symbol? (first sexp)) (app (first sexp) 
                                     (parse (second sexp)))]
        [ else (error 'parse "unexpected operator: ~a" (first sexp))]
     ))

;; sample program from Krishnamurthi p.29
;; NOTE: we pass the tree created by parse AND a list of function definitions
(interp (parse '{double {double 5}})
        (list (fundef 'double
                      'n
                      (add (id 'n) (id 'n)))))

(interp (parse '{sum-digits 6430761})
        (list (fundef 'sum-digits 'n ;; Add up the digits of n
              (parse '{if< n 10 ;; check if n is a single digit
                        n
                        {with {q {/ n 10}} ;; otherwise, pull off the last digit
                                           ;; and recurse
                           {with {r {- n {* q 10}}} 
                              {+ r {sum-digits q}} }}
                      })
      )))


;; list of function defs just play around with
(define dlist (list (fundef 'add2 'n
                            (add (id 'n) (num 2)))
                    (fundef 'five 'dummy
                            (num 5))
                    (fundef 'add4 'm
                            (app 'add2 (app 'add2 (id 'm))))
                    ))
  
; print nth fibonacci number recursively
(interp (parse '{fib 12})
        (list (fundef 'fib 'n
              (parse '{if< n 2
                        n
                        {+ {fib {- n 1}} {fib {- n 2}}}
                     })
     
        )))

; use fib to print an approximation for the golden ratio
; this works, but since we only have integer division, we get back 1
(interp (parse '{gold 20})
        (list (fundef 'fib 'n
              (parse '{if< n 2
                        n
                        {+ {fib {- n 1}} {fib {- n 2}}}
                     }))
              (fundef 'gold 'n
              (parse '{/ {fib {+ n 1}} {fib n}}
                     ))
        ))



;; our language doesn't include function definition (we have
;; to do that by hand in a separate list), but we can make
;; the separate list look a little more friendly by parsing
;; the function bodies:
;(define dlist (list (fundef 'add2 'n
;                            (parse '{+ n 2}))
;                    (fundef 'five 'dummy
;                            (parse '5))
;                    (fundef 'add4 'm
;                            (parse '{add2 {add2 m}}))
;                    ))