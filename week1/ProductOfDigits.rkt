#lang racket
(define (product-digits n)
  (cond
    [(= (quotient n 10) 0) n]
    [else (* (remainder n 10) (product-digits(quotient n 10)))]))
