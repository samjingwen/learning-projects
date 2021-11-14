#lang sicp
(#%require sicp-pict)

(define (compose f g)
  (lambda (x) (f (g x))))

(define (square n) (* n n))

(define (prime? n)
  (define (miller-rabin-expmod base exp m)
    (define (squaremod-with-check x)
      (define (check-nontrivial-sqrt1 x square)
        (if (and (= square 1)
                 (not (= x 1))
                 (not (= x (- m 1))))
            0
            square))
      (check-nontrivial-sqrt1 x (remainder (square x) m)))
    (cond ((= exp 0) 1)
          ((even? exp) (squaremod-with-check
                        (miller-rabin-expmod base (/ exp 2) m)))
          (else
           (remainder (* base (miller-rabin-expmod base (- exp 1) m))
                      m))))

  (define (miller-rabin-test n)
    (define (try-it a)
      (define (check-it x)
        (and (not (= x 0)) (= x 1)))
      (check-it (miller-rabin-expmod a (- n 1) n)))
    (try-it (+ 1 (random (- n 1)))))

  (define (fast-prime? n times)
    (cond ((= times 0) true)
          ((miller-rabin-test n) (fast-prime? n (- times 1)))
          (else false)))

  (fast-prime? n 40))

(cons 1
      (cons 2
            (cons 3
                  (cons 4 nil))))

(list 1 2 3 4)

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))

(list-ref squares 3)

(define (length items)
  (define (go count ptr)
    (cond ((null? ptr) count)
          (else (go (+ 1 count) (cdr ptr)))))
  (go 0 items))

(define odds (list 1 3 5 7))

(length odds)

; (define (append list1 list2)
;   (if (null? list1)
;       list2
;       (cons (car list1) (append (cdr list1) list2))))

; exercise 2.17

(define (last-pair lst)
  (define (go ptr)
    (cond ((null? (cdr ptr)) ptr)
          (else (go (cdr ptr)))))
  (go lst))

(last-pair squares)
(last-pair odds)

; exercise 2.18
(define (reverse lst)
  (define (go ptr res)
    (cond ((null? ptr) res)
          (else (go (cdr ptr) (cons (car ptr) res)))))
  (go lst nil))

(reverse odds)

; exercise 2.19

(define us-coins (list 50 25 10 5 1))

(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (first-denomination kinds-of-coins)
  (car kinds-of-coins))

(define (except-first-denomination kinds-of-coins)
  (cdr kinds-of-coins))

(define no-more? null?)


(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))

(define (us-count-change amount) (cc amount us-coins))
(define (uk-count-change amount) (cc amount uk-coins))

(us-count-change 100)
(uk-count-change 100)


; exercise 2.20

(define (same-parity . x)
  (let ((check? (if (odd? (car x))
                    odd?
                    even?)))
    (define (go ptr)
      (cond ((null? ptr) nil)
            ((check? (car ptr)) (cons (car ptr)
                                      (go (cdr ptr))))
            (else (go (cdr ptr)))))
    (cons (car x) (go (cdr x)))))

(define (same-parity-iter . x)
  (let ((check? (if (odd? (car x))
                    odd?
                    even?)))
    (define (go ptr res)
      (cond ((null? ptr) res)
            ((check? (car ptr)) (go (cdr ptr)
                                    (cons (car ptr) res)))
            (else (go (cdr ptr) res))))
    (reverse (go x nil))))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

(same-parity-iter 1 2 3 4 5 6 7)
(same-parity-iter 2 3 4 5 6 7)

; exericse 2.21

(define (square-list-1 items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list-1 (cdr items)))))

(define (square-list-2 items)
  (map square items))

(square-list-1 (list 1 2 3 4 5 6))
(square-list-2 (list 1 2 3 4 5 6))

; exercise 2.22

; http://community.schemewiki.org/?sicp-ex-2.22

(define (square-list-iter items)
  (define (iter l pick)
    (define r (square (car l)))
    (if (null? (cdr l))
        (pick (list r))
        (iter (cdr l) (lambda (x) (pick (cons r x))))))
  (iter items (lambda (x) x)))

(square-list-iter (list 1 2 3 4 5 6))

; exercise 2.23
(define (my-for-each proc lst)
  (cond ((not (null? lst)) (proc (car lst)) (my-for-each proc (cdr lst)))
        (else nil)))

(my-for-each (lambda (x)
               (newline)
               (display x)) (list 1 2 3 4))

(cons (list 1 2) (list 3 4))

(define x (cons (list 1 2) (list 3 4)))

(length x)

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(count-leaves x)

; exercise 2.24

(list 1 (list 2 (list 3 4)))


; exercise 2.25
(define x1 (list 1 3 (list 5 7) 9))
(define x2 (list (list 7)))
(define x3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(car (cdr (car (cdr (cdr x1)))))
(car (car x2))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr x3))))))))))))

; exercise 2.26
(define xx (list 1 2 3))
(define yy (list 4 5 6))

(append xx yy) ; (1 2 3 4 5 6)
(cons xx yy) ; ((1 2 3) 4 5 6)
(list xx yy) ; ((1 2 3) (4 5 6))

; exercise 2.27

(define (deep-reverse lst)
  (define (go ptr res)
    (cond ((null? ptr) res)
          ((not (pair? (car ptr)))
           (go (cdr ptr) (cons (car ptr) res)))
          (else (go (cdr ptr)
                    (cons (deep-reverse (car ptr))
                          res)))))
  (go lst nil))

(deep-reverse (list (list 1 2) (list 3 4)))
(deep-reverse (list (list 1 2 (list 1 2 3 4)) (list 3 4)))


; exercise 2.28
(define (fringe tree)
  (define (go ptr res)
    (cond ((null? ptr) res)
          ((not (pair? (car ptr))) (go (cdr ptr)
                                       (cons (car ptr) res)))
          (else (go (cdr ptr)
                    (go (car ptr) res)))))
  (reverse (go tree nil)))

(define x99 (list (list 1 2) (list 3 4)))
(fringe x99)
(fringe (list x99 x99))

; exercise 2.29

(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))

(define (branch-weight top-branch)
  (define (traverse branch weight)
    (cond ((null? branch) weight)
          ((not (pair? (branch-structure branch)))
           (+ weight (branch-structure branch)))
          (else (total-weight
                 (branch-structure branch)))))
  (traverse top-branch 0))

(define (total-weight mobile)
  (define (traverse branch weight)
    (cond ((null? branch) weight)
          ((not (pair? (branch-structure branch)))
           (+ weight (branch-structure branch)))
          (else (total-weight
                 (branch-structure branch)))))
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define m1
  (make-mobile (make-branch 2
                            (make-mobile (make-branch 3
                                                      (make-mobile (make-branch 4 4)
                                                                   (make-branch 4 3)))
                                         (make-branch 4
                                                      (make-mobile (make-branch 8 3)
                                                                   (make-branch 7 3)))))
               (make-branch 7 4)))

(total-weight m1)

(define (mobile-balanced? mobile)
  (= (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define m2
  (make-mobile (make-branch 2
                            (make-mobile (make-branch 3
                                                      (make-mobile (make-branch 4 4)
                                                                   (make-branch 4 3)))
                                         (make-branch 4
                                                      (make-mobile (make-branch 8 3)
                                                                   (make-branch 7 3)))))
               (make-branch 2
                            (make-mobile (make-branch 3
                                                      (make-mobile (make-branch 4 4)
                                                                   (make-branch 4 3)))
                                         (make-branch 4
                                                      (make-mobile (make-branch 8 3)
                                                                   (make-branch 7 3)))))))

(mobile-balanced? m1)
(mobile-balanced? m2)

; exercise 2.30

; (define (square-tree tree)
;   (cond ((null? tree) nil)
;         ((not (pair? tree)) (square tree))
;         (else (cons (square-tree (car tree)) (square-tree (cdr tree))))))

; (define (square-tree tree)
;   (map (lambda (sub-tree)
;          (if (not (pair? sub-tree))
;              (square sub-tree)
;              (square-tree sub-tree)))
;        tree))

; (square-tree
;  (list 1
;        (list 2 (list 3 4) 5)
;        (list 6 7)))

; exercise 2.31
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (not (pair? sub-tree))
             (proc sub-tree)
             (tree-map proc sub-tree)))
       tree))

(define (square-tree tree)
  (tree-map square tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

; exercise 2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets (list 1 2 3))

; http://community.schemewiki.org/?sicp-ex-2.32

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(filter odd? (list 1 2 3 4 5))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4 5))
(accumulate * 1 (list 1 2 3 4 5))
(accumulate cons nil (list 1 2 3 4 5))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 2 7)

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(enumerate-tree (list 1 (list 2 (list 3 4)) 5))

(define (sum-odd-squares tree)
  (accumulate
   + 0 (map square (filter odd? (enumerate-tree tree)))))

; (define (even-fibs n)
;   (accumulate
;    cons
;    nil
;    (filter even? (map fib (enumerate-interval 0 n)))))

; exercise 2.33

(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))
(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (my-length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(my-map square (list 1 2 3 4 5))
(my-append (list 1 2 3 4 5) (list 8 9 10))
(my-length (list 1 2 3 4 5 6))

; exercise 2.34
; (+ 1 (* 2 (+ 3 (* 2 (+ 0 (* 2 (+ 5 (* 2 (+ 0 (* 2 (+ 1 (* 2 0))))))))))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* 2 higher-terms)))
              0
              coefficient-sequence))

; 79
(horner-eval 2 (list 1 3 0 5 0 1))

; exercise 2.35
(define (my-count-leaves t)
  (accumulate +
              0
              (map (lambda (x) (if (pair? x)
                                   (my-count-leaves x)
                                   1))
                   t)))

(my-count-leaves x)

; exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(accumulate-n + 0 (list (list 1 2 3)
                        (list 4 5 6)
                        (list 7 8 9)
                        (list 10 11 12)))

; exercise 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (accumulate + 0 (accumulate-n * 1 (list v x))))
       m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (map (lambda (y) (dot-product x y))
                          cols))
         m)))

(dot-product (list 1 2 3)
             (list 4 5 6))
(matrix-*-vector (list (list 1 2 3) (list 4 5 6) (list 7 8 9))
                 (list 1 2 3))
(transpose (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(matrix-*-matrix (list (list 1 2 3)
                       (list 4 5 6))
                 (list (list 1 2)
                       (list 3 4)
                       (list 5 6)))

; exercise 2.38

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

; (fold-right / 1 (list 1 2 3)) 3/2
; (fold-left / 1 (list 1 2 3)) 1/6
; (fold-right list nil (list 1 2 3)) (1 (2 (3 ())))
; (fold-left list nil (list 1 2 3)) (((() 1) 2) 3)

; http://community.schemewiki.org/?sicp-ex-2.38


; exercise 2.39
(define (fold-right-reverse sequence)
  (accumulate (lambda (x y) (append y (list x))) nil sequence))
(define (fold-left-reverse sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

(fold-right-reverse (list 1 2 3 4))
(fold-left-reverse (list 1 2 3 4))


(accumulate
 append nil (map (lambda (i)
                   (map (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
                 (enumerate-interval 1 4)))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))


(define (permutations s)
  (if (null? s) ; empty set?
      (list nil) ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(permutations (list 1 2 3))

; exercise 2.40

(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(prime-sum-pairs 4)

; exercise 2.41
(define (ordered-triples n s)
  (map make-pair-sum
       (filter (lambda (x) (< (+ (car x) (cadr x)) s))
               (unique-pairs n))))

(ordered-triples 4 5)


; exercise 2.42
; http://community.schemewiki.org/?sicp-ex-2.42

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

(define empty-board nil)

(define (safe? board)
  (let ((queen (car board)))
    (define (iter rest-of-board right-diagonal left-diagonal)
      (cond
        ((null? rest-of-board) #t)
        ((= queen (car rest-of-board)) #f)
        ((= right-diagonal (car rest-of-board)) #f)
        ((= left-diagonal (car rest-of-board)) #f)
        (else
         (iter
          (cdr rest-of-board)
          (+ right-diagonal -1)
          (+ left-diagonal 1)))))
    (iter (cdr board) (+ queen -1) (+ queen 1))))

(queens 8)
(length (queens 8))

; exercise 2.43
; https://wernerdegroot.wordpress.com/2015/08/01/sicp-exercise-2-43/


; exercise 2.44

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))


; exercise 2.45
(define (split f g)
  (define (iter painter n)
    (if (= n 0)
        painter
        (let ((smaller (iter painter (- n 1))))
          (f painter (g smaller smaller)))))
  iter)

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
               (scale-vect (ycor-vect v) (edge2-frame frame))))))

; exercise 2.46

(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)

(define (proc-vect f a b)
  (make-vect (f (xcor-vect a) (xcor-vect b))
             (f (ycor-vect b) (ycor-vect b))))

(define (add-vect a b)
  (proc-vect + a b))
(define (sub-vect a b)
  (proc-vect - a b))
(define (scale-vect s b)
  (make-vect (* s (xcor-vect b))
             (* s (ycor-vect b))))

; exercise 2.47

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (caddr frame))

; (define (make-frame origin edge1 edge2)
;   (cons origin (cons edge1 edge2)))
; (define (origin-frame frame)
;   (car frame))
; (define (edge1-frame frame)
;   (cadr frame))
; (define (edge2-frame frame)
;   (cddr frame))

; exercise 2.48

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

; exercise 2.49

(define painter-a
  (segments->painter
   (list
    (segment (vect 0.0 0.0) (vect 0.0 1.0))
    (segment (vect 0.0 0.0) (vect 1.0 0.0))
    (segment (vect 0.0 1.0) (vect 1.0 1.0))
    (segment (vect 1.0 0.0) (vect 1.0 1.0)))))

(paint painter-a)

(define painter-b
  (segments->painter
   (list
    (segment (vect 0.0 0.0) (vect 1.0 1.0))
    (segment (vect 0.0 1.0) (vect 1.0 0.0)))))

(paint painter-b)

(define painter-c
  (segments->painter
   (list
    (segment (vect 0.5 0.0) (vect 0.0 0.5))
    (segment (vect 0.0 0.5) (vect 0.5 1.0))
    (segment (vect 0.5 1.0) (vect 1.0 0.5))
    (segment (vect 1.0 0.5) (vect 0.5 0.0)))))

(paint painter-c)

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (sub-vect (m corner1) new-origin)
                  (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (vect 0.0 1.0)
                     (vect 1.0 1.0)
                     (vect 0.0 0.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (vect 0.5 0.5)
                     (vect 1.0 0.5)
                     (vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (vect 1.0 0.0)
                     (vect 1.0 1.0)
                     (vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter painter
                     (vect 0.0 0.0)
                     (vect 0.65 0.35)
                     (vect 0.35 0.65)))

(define (beside painter1 painter2)
  (let ((split-point (vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter
            painter1
            (vect 0.0 0.0)
            split-point
            (vect 0.0 1.0)))
          (paint-right
           (transform-painter
            painter2
            split-point
            (vect 1.0 0.0)
            (vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

; exercise 2.50

(define (flip-horiz painter)
  (transform-painter painter
                     (vect 1.0 0.0)
                     (vect 0.0 0.0)
                     (vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (vect 1.0 1.0)
                     (vect 0.0 1.0)
                     (vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (vect 0.0 1.0)
                     (vect 0.0 0.0)
                     (vect 1.0 1.0)))

; exercise 2.51

(define (below painter1 painter2)
  (let ((split-point (vect 0.0 0.5)))
    (let ((paint-bottom
           (transform-painter
            painter1
            (vect 0.0 0.0)
            (vect 1.0 0.0)
            split-point))
          (paint-top
           (transform-painter
            painter2
            split-point
            (vect 1.0 0.5)
            (vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

(define (below painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))


; exercise 2.52

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (beside (below painter (up-split painter (- n 1)))
              (below (right-split painter (- n 1)) (corner-split painter (- n 1))))))

(define (corner-split-old painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-vert rotate180
                                  identity flip-horiz)))
    (combine4 (corner-split painter n))))

(define (square-limit-old painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(paint (square-limit einstein 2))
(paint (square-limit-old einstein 2))
