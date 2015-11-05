#lang racket
(define (series a b n)
  (define (series-iter a b i)
    (cond [(= n 1) a]
          [(> i n) b]
          [else (series-iter b (+ a b) (+ i 1))])
  )
  (series-iter a b 3))

(define (fibonacci n)
  (series 1 1 n))

(define (lucas n)
  (series 2 1 n))

(define (summed-member n)
  (+ (fibonacci n) (lucas n)))

(define (nth-fibonacci-sum n)
  (define (sum-iter i result)
    (cond [(> i n) result]
          [else (sum-iter (+ i 1) (+ result (fibonacci i) ))])
  )
  (sum-iter 1 0)
)

(define (nth-lucas-sum n)
  (define (sum-iter i result)
    (cond [(> i n) result]
          [else (sum-iter (+ i 1) (+ result (lucas i) ))])
  )
  (sum-iter 1 0)
)


(define (lucas-fib-diff n)
  (- (lucas n) (fibonacci n)))
