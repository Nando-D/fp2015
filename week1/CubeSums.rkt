#lang racket
(define (cube x) (* x x x))
(define (cube-sums? n)
   (define (cube-sums-iter? a b)
    (cond
      [(= n (+ (cube a) (cube b))) #t]
      [(< n (+ (cube a) (cube b))) #f]
      [(> n (cube(+ a 1))) (cube-sums-iter? (+ a 1) b)]
      [else (cube-sums-iter? a (+ b 1))]))
  (if(< n 2)
     #f
     (cube-sums-iter? 1 1))
  )
