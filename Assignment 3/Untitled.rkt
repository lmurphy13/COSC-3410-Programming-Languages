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
         (display (length sexp))
         (if (> (length sexp) 2) ; check to see if operator will have more than one operand
             (case (first sexp)
               [(+) (add (parse (second sexp))
                         (parse (third sexp)))]
               [(-) (sub (parse (second sexp))
                         (parse (third sexp)))]
               [(*) (mul (parse (second sexp))
                         (parse (third sexp)))]
               [(/) (div (parse (second sexp))
                         (parse (third sexp)))]
               [(with) (with (first (second sexp))
                             (parse (second (second sexp)))
                             (parse (third sexp)))]
               [(if<) (if< (parse (second sexp))
                           (parse (third sexp))
                           (parse (fourth sexp))
                           (parse (fifth sexp)))]
               [else (error 'parse "Unknown syntax")]
               )
               
             (case (first sexp)
             [(-) (minus (parse (second sexp)))]
             [else (error 'parse "Unknown syntax")]
             )
             
           )
        
   ]))


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
    [if< (expr1 expr2 val1 val2) (if (< (calc expr1) (calc expr2)) (calc val1) (calc val2))]
  ))


(define (run s)
  (calc (parse s)))