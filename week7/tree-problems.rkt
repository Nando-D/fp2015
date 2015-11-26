#lang racket

(define (make-tree node left right)
  (list node left right))

(define (make-leaf x)
  (make-tree x '() '()))

(define (empty-tree? tree)
  (empty? tree))

(define treee (make-tree 1
                         (make-tree 2 (make-leaf 4) (make-leaf 5))
                         (make-tree 3 '() (make-leaf 6))))
                                    
(define (root tree)
  (first tree))
(define (left tree)
  (first (rest tree)))
(define (right tree)
  (first (rest (rest tree))))

(define (count-nodes t)
  (cond [(empty-tree? t) 0]
        [else (+ 1
                 (count-nodes (left t))
                 (count-nodes (right t)))]))

(define (height-tree t)
  (cond [(empty-tree? t) 0]
        [(>= (count-nodes (left t)) (count-nodes (right t))) (count-nodes(left t))]
        [else (count-nodes(right t))]))

(define (height-tree2 t)
  (if(empty-tree? t)
     0
     (+ 1 (max (height-tree2(left t))
               (height-tree2(right t))))))

;TREE LEVELS

(define (tree-level level tree)
  (cond
    [(empty-tree? tree) '()]
    [(= level 1) (list (root tree))]
    [else (append (tree-level (- level 1) (left tree))
                  (tree-level (- level 1) (right tree)))]))

(define (tree-levels tree)
  (define h (height-tree2 tree))
  (define (helper level)
    (cond [(> level h) '()]
          [else (append (list (tree-level level tree)) (helper (+ level 1)))]))
  (helper 1))

;MAP A TREE

(define (tree-map f tree)
  (cond
    [(empty-tree? tree) '()]
    [else (make-tree (f (root tree)) (tree-map f (left tree)) (tree-map f (right tree)))]))

;BINARY SEARCH TREE

(define sorted-binary-tree (make-tree 8
                         (make-tree 7 (make-leaf 6) (make-leaf 10))
                         (make-tree 9 (make-leaf 2) (make-leaf 11))))

(define (bst-insert x tree)
  (cond
    [(empty-tree? tree) (make-tree x
                                   '()
                                   '())]
    [(< x (root tree)) (make-tree (root tree)
                                  (bst-insert x (left tree))
                                  (right tree))]
    [else (make-tree (root tree)
                     (left tree)
                     (bst-insert x (right tree)))]))
   
(define (bst-element? x tree)
  (cond
    [(empty-tree? tree) #f]
    [(= x (root tree)) #t]
    [(< x (root tree)) (bst-element? x (left tree))]
    [else (bst-element? x (right tree))]))

(define (bst->list tree)
  (cond
    [(empty-tree? tree) '()]
    [else (append (bst->list (left tree)) (list (root tree)) (bst->list (right tree)))]))

(define (bst? tree)
  (cond
    [(and (empty-tree? (right tree))
          (empty-tree? (left tree))) #t]
    [(or (< (root tree) (root (left tree)))
         (> (root tree) (root (right tree)))) #f]
    [else (and (bst? (left tree))
               (bst? (right tree)))]))
