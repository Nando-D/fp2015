#lang racket
(define (fst pair)
  (car pair))

(define (snd pair)
  (cdr pair))

(define (add-frac frac1 frac2)
  (if (or (= (snd frac1) 0) (= (snd frac2) 0)) "Incorrect input!"
     (simplify-frac (cons (+ (* (fst frac1) (snd frac2)) (* (fst frac2) (snd frac1))) (* (snd frac1) (snd frac2)))))
)

(define (substract-frac frac1 frac2)
  (if (or (= (snd frac1) 0) (= (snd frac2) 0)) "Incorrect input!"
  (simplify-frac (cons (- (* (fst frac1) (snd frac2)) (* (fst frac2) (snd frac1))) (* (snd frac1) (snd frac2))))
))

(define (mult-frac frac1 frac2)
  (cond [(or (= (snd frac1) 0) (= (snd frac2) 0)) "Incorrect input!" ]
        [(or (= (fst frac1) 0) (= (fst frac2) 0)) (cons 0 0) ]
        [else (simplify-frac (cons (* (fst frac1) (fst frac2)) (* (snd frac2) (snd frac1))))])
)

(define (simplify-frac frac)
  (cond [(= (fst frac) 0) (cons 0 0)]
        [(= (snd frac) 0) "Incorrect input!"]
        [else (cons (/ (fst frac) (gcd (fst frac) (snd frac))) (/ (snd frac) (gcd (fst frac) (snd frac))))])
)
