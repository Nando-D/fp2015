#lang racket
(require "Fence.rkt")

(define (nth-beast-number n)
  (if (<= n 0)
      0
      (string->number (string-repeat "666" n))))
