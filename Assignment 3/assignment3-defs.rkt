#lang plai

; Homework 3
; Written by Liam Murphy

(define-type WAE
  [num (n number?)]
  [id (name symbol?)]
  [add (lhs WAE?) (rhs WAE?)]
  [sub (lhs WAE?) (rhs WAE?)]
  [mul (lhs WAE?) (rhs WAE?)]
  [div (lhs WAE?) (rhs WAE?)]
  [minus (expr WAE?)]
  [with (name symbol?) (named-expr WAE?) (body WAE?)]
  [if< (expr1 WAE?) (expr2 WAE?) (val1 WAE?) (val2 WAE?)]
 )

;; parse : sexp -> WAE
;; to convert s-expressions into WAEs
(define (parse sexp)
  (cond [(number? sexp) (num sexp)]
        [(symbol? sexp) (id sexp)]
        [(list? sexp)
         (cond [(null? sexp) (error 'parse "Cannot parse expression with zero arguments!")]) ; empty expression check: {}
         (case (first sexp)
           [(+) (cond [(= (length sexp) 3) (add (parse (second sexp))
                                               (parse (third sexp)))]
                      [else (error 'parse "(+) illegal parameter count: parameter count does not match known definitions!")])]
           [(-) (cond [(= (length sexp) 3) (sub (parse (second sexp))
                                                (parse (third sexp)))]
                      [(= (length sexp) 2) (minus (parse (second sexp)))]
                      [else (error 'parse "(-) illegal parameter count: parameter count does not match known definitions!")])]
           [(*) (cond [(= (length sexp) 3) (mul (parse (second sexp))
                                                (parse (third sexp)))]
                      [else (error 'parse "(*) illegal parameter count: parameter count does not match known definitions!")])]
           [(/) (cond [(= (length sexp) 3) (div (parse (second sexp))
                                               (parse (third sexp)))]
                      [else (error 'parse "(/) illegal parameter count: parameter count does not match known definitions!")])]
           [(with) (cond [(and (= (length sexp) 3) (= (length (second sexp)) 2)) (with (first (second sexp))
                                                                                       (parse (second (second sexp)))
                                                                                       (parse (third sexp)))]
                         [else (error 'parse "(with) illegal parameter count: parameter count does not match known definitions!")])]
           [(if<) (cond [(= (length sexp) 5) (if< (parse (second sexp))
                                                  (parse (third sexp))
                                                  (parse (fourth sexp))
                                                  (parse (fifth sexp)))]
                        [else (error 'parse "(if<) illegal parameter count: parameter count does not match known definitions!")])]
           [else (error 'parse "Unknown syntax!")]
          )
         ]
        )
  )



;subst: WAE symbol WAE --> WAE
;
; Example:
; > (subst (with 'y (add (id 'x) (num 2)) (add (id 'y)(id 'x))) 'x (num 5))
; (with 'y (add (num 5) (num 2)) (add (id 'y) (num 5)))
;
(define (subst expr sub-id val)
  (type-case WAE expr
    [num (n) expr]
    [add (l r) (add (subst l sub-id val)
                    (subst r sub-id val))]
    [sub (l r) (sub (subst l sub-id val)
                    (subst r sub-id val))]
    [mul (l r) (mul (subst l sub-id val)
                    (subst r sub-id val))]
    [div (l r) (div (subst l sub-id val)
                    (subst r sub-id val))]
    [minus (n) (minus (subst n sub-id val))]
    [with (bound-id named-expr bound-body)
          (if (symbol=? bound-id sub-id)
              (with bound-id
                    (subst named-expr sub-id val) ; we need to subst into the named-expr
                    bound-body)
              (with bound-id
                    (subst named-expr sub-id val) ; we need to subst into the named-expr
                    (subst bound-body sub-id val)))] ; as well as the body.
    [id (v) (if (symbol=? v sub-id) val expr)]
    [if< (expr1 expr2 val1 val2) (if< (subst expr1 sub-id val)
                                      (subst expr2 sub-id val)
                                      (subst val1 sub-id val)
                                      (subst val2 sub-id val))]
 ))

; calc: WAE --> number
(define (calc expr)
  (type-case WAE expr
    [num (n) n]
    [add (l r) (+ (calc l) (calc r))]
    [sub (l r) (- (calc l) (calc r))]
    [mul (l r) (* (calc l) (calc r))]
    [div (l r) (/ (calc l) (calc r))]
    [minus (n) (* (calc n) -1)]
    [with (bound-id named-expr bound-body)
          (calc (subst bound-body
                       bound-id
                       (num (calc named-expr))))]
    [id (v) (error 'calc "free identifier ~a" v)]
    [if< (expr1 expr2 val1 val2) (if (< (calc expr1) (calc expr2)) (calc val1) (calc val2))] ; checks if expr1 is less than expr2.
                                                                                             ; if so, return val1, otherwise return val2
  ))

; run: S-expression --> number
; used to parse and calc CWAE programs
(define (run s)
  (calc (parse s)))



; Test case 1
; Calculating the area of a circle with a radius of 7
; {with {pi 3.14159}
;       {with {r 7}
;             {* pi {* r r}}
; }}

; Test case 2
; with {a 5}
;      {with {b 40}
;            {if< {* a a} {- b} {- a b} {/ b a}}
;  }}