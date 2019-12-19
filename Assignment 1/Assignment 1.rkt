#lang racket

; Assignment 1
; Written by: Liam Murphy

; Problem 1
; beginning?: list list ---> boolean
(define beginning?
  (lambda (list-a list-b)
    (cond [(null? list-a) #t] ; is list_a null?. if it is, return true
          [(null? list-b) #f] ; if list_a isn't null, check if list_b is null. if it is, return false
          [(equal? (car list-a) (car list-b)) (beginning? (cdr list-a) (cdr list-b))] ; if first atom in each list are equal, then continue
          [else #f] ; otherwise, we know the first two atoms are not equal, thus return false
     )))

; Problem 2
; sum-evens?: list ---> integer
(define sum-evens
  (lambda (nums)
    (cond [(null? nums) 0] ; is nums null? if so, return 0
          [(even? (car nums)) (+ (car nums) (sum-evens (cdr nums)))] ; is (car nums) even? if so, add (car nums) with the next recursion of the rest of nums
          [else (sum-evens (cdr nums))] ; if we get here, that means (car nums) was odd. therefore, let's continue to the next atom
     )))


; Problem 3
; position: atom list ---> integer
(define position
  (lambda (atm lst)
    (cond [(null? lst) 0] ; is lst null? if so, return 0 (aka we haven't found atm in lst)
          [(equal? atm (car lst)) 1] ; is atm the first atom in lst
          [(= (position atm (cdr lst)) 0) 0] ; is position of atm equal to zero? if so, return 0
          [else (+ (position atm (cdr lst)) 1)] ; otherwise, we add 1 to the next recursive call of position
     )))