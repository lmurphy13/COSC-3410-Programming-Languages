#lang racket
; Assignment 1
; Written by: Liam Murphy


; Problem 3
; position: atom list ---> integer
(define position
  (lambda (atm lst)
    (cond [(null? lst) 0] ; is lst null? if so, return 0 (aka we haven't found atm in lst)
          [(equal? atm (car lst)) 1] ; is atm the first atom in lst
          [(= (position atm (cdr lst)) 0) 0] ; is position of atm equal to zero? if so, return 0
          [else (+ (position atm (cdr lst)) 1)] ; otherwise, we add 1 to the next recursive call of position
     )))