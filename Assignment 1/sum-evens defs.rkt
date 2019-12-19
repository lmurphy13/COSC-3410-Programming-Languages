#lang racket
; Assignment 1
; Written by: Liam Murphy

; Problem 2
; sum-evens?: list ---> integer
(define sum-evens
  (lambda (nums)
    (cond [(null? nums) 0] ; is nums null? if so, return 0
          [(even? (car nums)) (+ (car nums) (sum-evens (cdr nums)))] ; is (car nums) even? if so, add (car nums) with the next recursion of the rest of nums
          [else (sum-evens (cdr nums))] ; if we get here, that means (car nums) was odd. therefore, let's continue to the next atom
     )))