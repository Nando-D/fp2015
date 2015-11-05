#lang racket
(define (string-reverse str)
  (define (string-reverse-iter strrev i)
    (if(< i 0)
       strrev
       (string-reverse-iter (string-append strrev (~a (string-ref str i))) (- i 1))))
  (define length (- (string-length str) 1))
  (string-reverse-iter "" length)
)



(define (to-binary-string n)
  (define (to-binary-string-iter result curr)
    (if (= curr 0)
        result
        (to-binary-string-iter (string-append result (~a (remainder curr 2))) (quotient curr 2))))
  (if (< n 0) (~a 0)
              (string-reverse (to-binary-string-iter "" n)))
)


(define (from-binary-string binary-str)
  (define (from-binary-string-iter result i curr)
    (if (= curr 0)
        result
        (from-binary-string-iter  (+ result (* (remainder curr 10) (expt 2 i))) (+ i 1) (quotient curr 10))))
  (if (< (string->number binary-str) 0)
      0
      (from-binary-string-iter 0 0 (string->number binary-str))))
