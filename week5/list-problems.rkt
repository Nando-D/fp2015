#lang racket

; Намира сумата на всички числа в numbers
; -> (sum (list))
; 0
; -> (sum (list 1 2 3))
; 6

(define (sum numbers)
  (if(empty? numbers)
     0
     (+ (first numbers) (sum (rest numbers)))))

; Проверява дали x се среща в items
; -> (member? 1 (list 1 2 3))
; #t
; -> (member? "asdf" (list "asd"))
; #f
; Разгледайте http://docs.racket-lang.org/reference/booleans.html

(define (member? x items)
  (cond
    [(empty? items) #f]
    [(equal? (first items) x) #t]
    [else (member? x (rest items))]
   ))

; -> (length2 (range2 1 10))
; 9
; В Racket има такава функция, наречена length

(define (length2 items)
  (cond
    [(empty? items) 0]
    [else (+ 1 (length2 (rest items)))]))

; Връща n-тия елемент от items при 0лево базиран индекс
; -> (list-ref2 (list 1 2 3) 0)
; 1
; В Racket има такава функция, наречена list-ref

(define (list-ref2 items n)
  (cond
    [(zero? n) (first items)]
    [else (list-ref2 (rest items) (- n 1))]))

; -> (range2 1 10)
; '(1 2 3 4 5 6 7 8 9)
; В Racket съществува такава функция, наречена range

(define (range2 a b)
  (cond
    [(>= a b) '()]
    [else (cons a (range2 (+ a 1) b))]))

; Строи списък от числата между 0 и n, включително, като прилага f върху всяко число
; i-тия елемент на този списък е (f i)
; -> (build-list2 3 id)
; '(0 1 2)
; -> (build-list2 3 (lambda (x) (* x x)))
; '(0 1 4)
; В Racket има такава функция, наречена build-list

(define (build-list2 n f)
  (map f (range 0 n)))
  
; Конкатенира два списъка в нов списък
; -> (append2 (list 1 2 3) (list 4 5 6))
; '(1 2 3 4 5 6)
; В Racket има такава фунцкия, наречена append

(define (append2 l1 l2)
  (cond
    [(empty? l1) l2]
    [else (cons (first l1) (append2 (rest l1) l2))]))

; Обръща списъка наобратно
; -> (reverse2 (list 1 2 3))
; '(3 2 1)
; В Racket има такава функция, наречена reverse

(define (reverse2 items)
  (cond
    [(empty? items) '()]
    [else (append (reverse2 (rest items)) (list (first items)))]))

; Взима първите n елемента от даден списък
; Ако (> n (length items)), тогава връща items
; -> (take2 3 (list 1 2 3 4 5))
; '(1 2 3)

(define (take2 n items)
  (cond
    [(> n (length items)) items]
    [(= n 0) '()]
    [else (append (list (first items)) (take2 (- n 1) (rest items)))]))

; Маха първите n елемента от даден списък
; Ако (> n (length items)) връща '()
; -> (drop2 3 (list 1 2 3 4 5))
; '(4 5)

(define (drop2 n items)
  (cond
    [(> n (length items)) '()]
    [(= n 0) items]
    [else (drop2 (- n 1) (rest items))]))

; Функция от по-висок ред. Взима поредни елементи от items докато предиката p за тях дава истина
; -> (take-while zero? (list 0 0 0 1 2 3))
; '(0 0 0)
; -> (take-while even? (list 2 4 5 7 8 3 2))
; '(2 4)
; -> (take-while (lambda (x) (> x 3)) (list 1 1 1 1 1))
; '()

(define (take-while p items)
  (cond
    [(or (not (p (first items))) (empty? items)) '()]
    [else (cons (first items) (take-while p (rest items)))]))
    
; Функция от по-висок ред. Маха поредните елементи от items докато предикатa p дава лъжа за тях
; -> (drop-while zero? (list 0 0 0 1 2 3))
; '(1 2 3)
; -> (drop-while even? (list 2 4 5 7 8 3 2))
; '(5 7 8 3 2)
; -> (drop-while (lambda (x) (> x 3)) (list 1 1 1 1 1))
; '(1 1 1 1 1)

(define (drop-while p items)
  (cond 
    [(empty? items) '()]
    [(not (p (first items))) items] 
    [else (drop-while p (rest items))]))

; Функцията взима число и връща списък от цифрите му
; -> (number->list 123)
; '(1 2 3)

(define (number->list n)
  (if (= n 0) '()
      (append (number->list (quotient n 10)) (list (remainder n 10)))))

; Функцията взима списък от цифри и връща числото
; -> (list->number (list 1 2 3))
; 123

(define (list->number ns)
  (if (empty? ns) 0
      (+ (* (first ns) (expt 10 (- (length ns) 1))) (list->number (rest ns)))))
      
