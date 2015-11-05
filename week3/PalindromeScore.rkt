#lang racket


(define (reverse-int n)
  (define (rev-iter n result)
    (cond [(= n 0) result]
          [else (rev-iter (quotient n 10) (+ (* result 10) (remainder n 10)))]))
  (rev-iter n 0))

(define (palindrome? n)
  (= n (reverse-int n)))

(define (p-score n)
  (define (p-score-iter _n i)
    (if(palindrome? _n)
       i
       (p-score-iter (+ _n (reverse-int _n)) (+ i 1))))
  (p-score-iter n 1))
       
