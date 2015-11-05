#lang racket
(provide string-repeat)

(define (string-repeat str n)
  (define (string-repeat-iter result i)
     (if (>= i n)
        result
        (string-repeat-iter (string-append result str) (+ i 1))))
  (string-repeat-iter "" 0))


(define (fence n)
  (string-append "{" (string-repeat "-" (round (+ 1 (log n)))) ">"  (~a n)  "<" (string-repeat "-" (round (+ 1 (log n)))) "}")
)
