#lang racket
(provide take-while)
(define (take-while p items)
  (cond
    [(empty? items) '()]
    [(not (p (first items))) '()]
    [else (cons (first items) (take-while p (rest items)))]))

(provide drop-while)
(define (drop-while p items)
  (cond 
    [(empty? items) '()]
    [(not (p (first items))) items] 
    [else (drop-while p (rest items))]))
