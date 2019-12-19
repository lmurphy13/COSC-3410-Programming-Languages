#lang racket
; Assignment 1
; Written by Liam Murphy

; Problem 1
; beginning?: list list ---> boolean
(define beginning?
  (lambda (list-a list-b)
    (cond [(null? list-a) #t] ; is list_a null?. if it is, return true
          [(null? list-b) #f] ; if list_a isn't null, check if list_b is null. if it is, return false
          [(equal? (car list-a) (car list-b)) (beginning? (cdr list-a) (cdr list-b))] ; if first atom in each list are equal, then continue
          [else #f] ; otherwise, we know the first two atoms are not equal, thus return false
     )))