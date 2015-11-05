#lang racket
;(require "PrimeNumber.rkt")
;(define (square x)
       ;(* x x))

(define (f p g h)
  (lambda (x) (and (p (g x)) (p (h x)))))
