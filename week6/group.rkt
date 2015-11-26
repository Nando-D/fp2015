#lang racket
(require "TakeAndDropWhile.rkt")

(define (group-list items)
    (if(empty? items)
       '()
       (cons (take-while (lambda (x) (equal? x (first items))) items)
                (group-list (drop-while (lambda(x) (equal? x (first items))) items)))))
