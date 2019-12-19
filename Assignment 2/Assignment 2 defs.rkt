#lang racket
; Assignment 2
; Written by: Liam Murphy

; atom definition from The Little Schemer
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))
   ))

; Problem 1
; differences: list --> list
(define differences
  (lambda (s)
    (cond [(null? s) '()] ; Is s null? If so, return the empty list
          [(= (length s) 1) '()] ;  Is the length of s equal to 1? If so, return the empty list
          [else (cons (- (car (cdr s)) (car s)) (differences (cdr s)))] ; If the length of s is not 1, cons the difference of the cdr of s and the car of s
          ; with the recursive call of difference on the cdr of s
     )))

; Problem 2
; merge: list list --> list
(define merge
  (lambda (lst-a lst-b)
    (cond [(null? lst-a) '()] ; Is lst-a null? If so, return the empty list
          [(null? lst-b) '()] ; Is lst-b null? If so, return the empty list
          [(< (length lst-a) (length lst-b)) (error 'merge "first list too short")] ; Is the length of lst-a less than the length of lst-b? If so, throw an error
          [(< (length lst-b) (length lst-a)) (error 'merge "second list too short")] ; Is the length of lst-b less than the length of lst-a? If so, throw an error
          [else (append (append (list (car lst-a)) (list (car lst-b))) (merge (cdr lst-a) (cdr lst-b)))]
          ; If the lengths are the same, append the car of lst-a (as a list) to the car of lst-b (as a list).
          ; Then, append the result to the recursive call of merge on the cdr of lst-a and the cdr of lst-b
    )))

; Problem 3
; deep-add: list --> integer
(define deep-add
  (lambda (s)
    (cond [(null? s) 0] ; Is s null? If so, return the empty list
          [(number? (car s)) (+ (car s) (deep-add (cdr s)))] ; Is the car of s a number? If so, add it to the recursive call of deep-add on the cdr of s
          [(atom? (car s)) (deep-add (cdr s))] ; Is the car of s an atom? If so, call deep-add on the cdr of s
          [else (+ (deep-add (cdr s)) (deep-add (car s)))] ; Otherwise, add the call of deep-add on the cdr of s to the call of deep-add on the car of s
     )))


; Problem 4
; shape: list --> list
(define shape
  (lambda (s)
    (cond [(null? s) '()] ; Is s null? If so, return the empty list?
          [(atom? (car s)) (shape (cdr s))] ; Is the car of s an atom? If so, call shape on the cdr of s
          [else (cons (shape (car s)) (shape (cdr s)))] ; Otherwise, cons the call of shape on the car of s to the call of shape on the cdr of s
     )))