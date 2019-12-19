 #lang plai

; Assignemnt 7
; Written by Liam Murphy

(define-type BOB
  [num (n number?)]
  [id (name symbol?)]
  [add (lhs BOB?) (rhs BOB?)]
  [sub (lhs BOB?) (rhs BOB?)]
  [mul (lhs BOB?) (rhs BOB?)]
  [div (lhs BOB?) (rhs BOB?)]
  [minus (expr BOB?)]
  [equal (lhs BOB?) (rhs BOB?)]
  [less (lhs BOB?) (rhs BOB?)]
  [with (name symbol?) (named-expr BOB?) (body BOB?)]
  [iff (test-val BOB?) (true-val BOB?) (false-val BOB?)]
  [fun (arg-name symbol?) (body BOB?)]
  [rec (bound-id symbol?)
       (named-expr BOB?)
       (bound-body BOB?)]
  [app (fun-expr BOB?) (arg BOB?)])

(define-type BOB-value
  [numV (n number?)]
  [booleanV (n boolean?)]
  [closureV (param symbol?)
            (body BOB?)
            (env Env?)]
  )

(define (boxed-BOB-value? b)
  (and (box? b)
       (BOB-value? (unbox b)))
  )

(define-type Env
  [mtSub]
  [aSub (name symbol?) (value BOB-value?) (env Env?)]
  [aRecSub (name symbol?)
          (value boxed-BOB-value?)
          (env Env?)]
  )

;;cyclically-bind-and-interp : symbol fun env --> env
(define (cyclically-bind-and-interp bound-id named-expr env)
  (let* ((value-holder (box (numV 55)))
         (new-env (aRecSub bound-id value-holder env))
         (named-expr-val (interp named-expr new-env)))
     (set-box! value-holder named-expr-val)
     new-env
  ))

;; lookup: symbol Env --> BOB
(define (lookup name env)
  (type-case Env env
    [mtSub () (error 'lookup "no binding found for id ~a" name)]
    [aSub (bound-name bound-value rest-env)
          (if (symbol=? name bound-name)
              bound-value
              (lookup name rest-env))]
    [aRecSub (bound-name boxed-value rest-env)
          (if (symbol=? name bound-name)
              (unbox boxed-value)
              (lookup name rest-env))]
 ))

;; tree-add: BOB-value BOB-value --> BOB-value
(define (tree-add a b)
  (numV (+ (numV-n a) (numV-n b)))
 )

(define (tree-mul a b)
  (numV (* (numV-n a) (numV-n b)))
 )
(define (tree-sub a b)
  (numV (- (numV-n a) (numV-n b)))
 )
(define (tree-div a b)
  (numV (quotient (numV-n a) (numV-n b)))
 )
(define (tree-minus n)
  (numV (* (numV-n n) -1))
 )
(define (tree-equal a b)
  (booleanV (eq? (numV-n a) (numV-n b)))
 )
(define (tree-less a b)
  (booleanV (< (numV-n a) (numV-n b)))
 )

;; interp: BOB Env --> BOB-value
(define (interp expr env)
  (type-case BOB expr
    [id (v) (lookup v env)]
    [num (n) (numV n)]
    [add (l r) (tree-add (interp l env) (interp r env))]
    [sub (l r) (tree-sub (interp l env) (interp r env))]
    [mul (l r) (tree-mul (interp l env) (interp r env))]
    [div (l r) (tree-div (interp l env) (interp r env))]
    [minus (n) (tree-minus (interp n env))]
    [equal (l r) (tree-equal (interp l env) (interp r env))]
    [less (l r) (tree-less (interp l env) (interp r env))]
    
    [with (bound-id named-expr bound-body)
          (interp bound-body
                  (aSub bound-id
                        (interp named-expr env)
                        env))]
    [rec (bound-id named-expr bound-body)
      (interp bound-body
              (cyclically-bind-and-interp bound-id
                                          named-expr
                                          env))]
    [iff (tst tv fv)
         (cond [(numV? (interp tst env))
                (if (zero? (numV-n (interp tst env))) (interp tv env)
                    (interp fv env))]
               [(booleanV? (interp tst env))
                (if (boolean? (booleanV-n (interp tst env))) (interp tv env)
                     (interp fv env))])]
    [fun (arg body)(closureV arg body env)]
    [app (fun-expr arg-expr)
         (let ((fun-closure (interp fun-expr env)))
           (interp (closureV-body fun-closure)
                          (aSub (closureV-param fun-closure)
                                (interp arg-expr env)
                                (closureV-env fun-closure))))]
  ))

;; parse: s-expr -->BOB
(define (parse sexp)
  (cond [(number? sexp) (num sexp)]
        [(symbol? sexp) (id sexp)]
        [(eq? (first sexp) '+) (cond [(= (length sexp) 3) (add (parse (second sexp))
                                                               (parse (third sexp)))]
                                     [else (error 'parse "(+) illegal parameter count: parameter count does not match known definitions!")])]
        [(eq? (first sexp) '*) (cond [(= (length sexp) 3) (mul (parse (second sexp))
                                                               (parse (third sexp)))]
                                     [else (error 'parse "(*) illegal parameter count: parameter count does not match known definitions!")])]
        [(eq? (first sexp) '-) (cond [(= (length sexp) 3) (sub (parse (second sexp))
                                                               (parse (third sexp)))]
                                     [(= (length sexp) 2) (minus (parse (second sexp)))]
                                     [else (error 'parse "(-) illegal parameter count: parameter count does not match known definitions!")])]
        [(eq? (first sexp) '/) (cond [(= (length sexp) 3) (div (parse (second sexp))
                                                               (parse (third sexp)))]
                                     [else (error 'parse "(/) illegal parameter count: parameter count does not match known definitions!")])]
        [(eq? (first sexp) '<) (less (parse (second sexp))
                                     (parse (third sexp)))]
        [(eq? (first sexp) '=) (equal (parse (second sexp))
                                      (parse (third sexp)))]
        [(eq? (first sexp) 'with) (app (fun (car (second sexp))
                                            (parse (third sexp)))
                                       (parse (second (second sexp))))]
        [(eq? (first sexp) 'rec) (rec (first (second sexp))
                                    (parse (second (second sexp)))
                                    (parse (third sexp)))]
        [(eq? (first sexp) 'fun) (fun (car (second sexp))
                                      (parse (third sexp)))]
        [(eq? (first sexp) 'iff) (iff (parse (second sexp))
                                      (parse (third sexp))
                                      (parse (fourth sexp)))]
        [else (app (parse (first sexp)) 
                   (parse (second sexp)))]
     ))
;(define (fac n)
;  (cond [(<= n 1) 1]
;        [else (* n (fac (- n 1)))]
;  ))
(define fac-ex (parse
        '{rec {fac {fun {n}
              {iff n 1
                  {* n {fac {+ n -1}}}}}}
           
                       {fac 6}}))
(interp fac-ex (mtSub))
  
;(define (fib n)
;  (cond ((<= n 2) 1)
;        (else (+ (fib (- n 1)) (fib (- n 2))))
;  ))
;(fib 10)

;(define (fac n)
;  (cond [(<= n 1) 1]
;        [else (* n (fac (- n 1)))]
;  ))
;(fac 5)