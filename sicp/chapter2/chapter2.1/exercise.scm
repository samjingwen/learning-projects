#lang sicp
(#%require (lib "27.ss" "srfi"))

(define (println output)
  (newline)
  (display output)
  (newline))

(define (square x) (* x x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

; (define make-rat cons)
(define numer car)
(define denom cdr)

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

; (define (make-rat n d)
;   (let ((g (gcd n d)))
;     (cons (/ n g) (/ d g))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; (print-rat (make-rat 1 2))
; (print-rat (make-rat 1 2))
; (print-rat (add-rat (make-rat 1 2) (make-rat 1 3)))
; (print-rat (add-rat (make-rat 1 3) (make-rat 1 3)))

; exercise 2.1

(define (make-rat n d)
  (let ((g (gcd n d)))
    (let ((nn (/ n g))
          (dd (/ d g)))
      (cond ((and (> nn 0) (< dd 0))
             (cons (- nn) (- dd)))
            (else (cons (abs nn) (abs dd)))))))

(print-rat (make-rat -3 6))
(print-rat (make-rat 1 -3))
(print-rat (make-rat -3 -9))
(print-rat (make-rat 1 3))


; exercise 2.2

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define make-point cons)
(define x-point car)
(define y-point cdr)

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define (midpoint-segment n)
  (let ((a (start-segment n))
        (b (end-segment n)))
    (let ((xa (x-point a))
          (ya (y-point a))
          (xb (x-point b))
          (yb (y-point b)))
      (make-point (/ (+ xa xb) 2)
                  (/ (+ ya yb) 2)))))

(let ((start (make-point 3 1))
      (end (make-point 2 2)))
  (let ((seg (make-segment start end)))
    (print-point (midpoint-segment seg))))

; exercise 2.3

(define make-rect cons)

(define bottom-left car)
(define top-right cdr)

(define side-1 car)
(define side-2 cdr)

(define (point-dist p1 p2)
  (sqrt (+ (square (- (x-point p1) (x-point p2)))
           (square (- (y-point p1) (y-point p2))))))

(define (seg-len seg)
  (point-dist (start-segment seg)
              (end-segment seg)))

(define p1 (make-point 1 3))
(define p2 (make-point 3 3))
(define rect1 (make-rect p1 p2))

(define (side-lengths rect)
  (cons (seg-len (side-1 rect))
        (min (point-dist (start-segment (side-1 rect))
                         (start-segment (side-2 rect)))
             (point-dist (start-segment (side-1 rect))
                         (end-segment (side-2 rect))))))

(define (width-rect rect)
  (car (side-lengths rect)))

(define (height-rect rect)
  (cdr (side-lengths rect)))

(define (area-rect rect)
  (* (width-rect rect) (height-rect rect)))
(define (perimeter-rect rect)
  (* (+ (width-rect rect) (height-rect rect)) 2))

(area-rect (make-rect (make-segment (make-point 0 1)
                                    (make-point 0 0))
                      (make-segment (make-point 1 0)
                                    (make-point 1 1))))
(perimeter-rect (make-rect (make-segment (make-point 0 1)
                                         (make-point 0 0))
                           (make-segment (make-point 1 0)
                                         (make-point 1 1))))

; exercise 2.4

(println "***2.4")

(define (new-cons x y)
  (lambda (m) (m x y)))

(define (new-car z)
  (z (lambda (p q) p)))

(define (new-cdr z)
  (z (lambda (p q) q)))

(define var1 (new-cons 1 2))
(new-car var1)
(new-cdr var1)

; exercise 2.5

(println "***2.5")

(define (expt base n)
  (define (go counter product)
    (cond ((= counter n) product)
          ((even? n) (go (+ counter 2) (* product (square base))))
          (else (go (+ counter 1) (* product base)))))
  (go 0 1))

(define (count-successful-division num div)
  (define (go curr res)
    (cond ((= (remainder curr div) 0) (go (/ curr div) (+ res 1)))
          (else res)))
  (go num 0))

(define (exp-cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (exp-car z)
  (count-successful-division z 2))

(define (exp-cdr z)
  (count-successful-division z 3))

(define pair (exp-cons 99 128))
(exp-car pair)
(exp-cdr pair)

; exercise 2.6

(println "***2.6")

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; (add-1 zero)
; (lambda (f) (lambda (x) (f ((n f) x))))
; (lambda (f) (lambda (x) (f (((lambda (f1) (lambda (x1) x1)) f) x))))
; (lambda (f) (lambda (x) (f ((lambda (x1) x1) x))))
; (lambda (f) (lambda (x) (f x)))

(define one (lambda (f) (lambda (x) (f x))))

; (add-1 one)
; (lambda (f) (lambda (x) (f (((lambda (f1) (lambda (x1) (f1 x1))) f) x))))
; (lambda (f) (lambda (x) (f ((lambda (x1) (f x1)) x))))
; (lambda (f) (lambda (x) (f (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add a b) (lambda (f) (lambda (x) ((b f) ((a f) x)))))

; exercise 2.7

(println "***2.7")

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (old-mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (<= (* (lower-bound y) (upper-bound y)) 0)
      (error "Interval spans 0")
      (old-mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))

(define (make-interval a b)
  (if (< a b)
      (cons a b)
      (cons b a)))

(define (lower-bound z) (car z))

(define (upper-bound z) (cdr z))

; exercise 2.8

(println "***2.8")

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define x1 (make-interval -100 100))
(define x2 (make-interval -100 -10))

(sub-interval x1 x2)

(define x3 (make-interval 100 200))
(define x4 (make-interval 10 100))

(sub-interval x3 x4)

; exercise 2.9
; http://community.schemewiki.org/?sicp-ex-2.9

; exercise 2.10

(println "***2.10")

(define xx1 (make-interval 0 10))
(define xx2 (make-interval -10 10))
(define xx3 (make-interval 1 10))

; (div-interval xx1 xx2)
; (div-interval xx1 xx3)

; exercise 2.11
; http://community.schemewiki.org/?sicp-ex-2.11

(println "***2.11")

(define (generate-intervals)
  (define test-list '())
  (define test-data
    (cons (list 0 1 2 3 4 5 -6 -7 -8 -9 -10)
          (list 5 4 3 2 1 0 -1 -2 -3 -4 -5)))
  (for-each
   (lambda (x) (set! test-list (append test-list x)))
   (map    (lambda (x)     (map    (lambda (y) (make-interval x y))
                                   (cdr test-data)))
           (car test-data)))
  (cons test-list test-list))

(define test-intervals
  (generate-intervals))

(define (test f g)
  (define (interval-equals a b)
    (and (= (lower-bound a) (lower-bound b)) (= (upper-bound a) (upper-bound b))))
  (for-each (lambda (x)
              (for-each (lambda (y)
                          (cond   ((interval-equals (f x y) (g x y)) #t)
                                  (else
                                   (newline)
                                   (display x) (display y)
                                   (newline)
                                   (display (f x y)) (display (g x y))
                                   (newline))))
                        (cdr test-intervals)))
            (car test-intervals)))

(define (mul-interval x y)
  (define (opposite-pair? a b)
    (if (positive? a)
        (negative? b)
        (positive? b)))

  (define (positive-pair? a b)
    (if (opposite-pair? a b)
        #f
        (positive? a)))

  (define (negative-pair? a b)
    (if (opposite-pair? a b)
        #f
        (negative? a)))
  (let        ((x0 (lower-bound x))
               (x1 (upper-bound x))
               (y0 (lower-bound y))
               (y1 (upper-bound y)))
    (cond   ((negative-pair? x0 x1)
             (cond   ((opposite-pair? y0 y1)
                      (make-interval (* x0 y0) (* x0 y1)))
                     ((negative-pair? y0 y1)
                      (make-interval (* x1 y1) (* x0 y0)))
                     (else
                      (make-interval (* x1 y0) (* x0 y1)))))
            ((positive-pair? x0 x1)
             (cond   ((opposite-pair? y0 y1)
                      (make-interval (* x1 y0) (* x1 y1)))
                     ((negative-pair? y0 y1)
                      (make-interval (* x1 y0) (* x0 y1)))
                     (else
                      (make-interval (* x0 y0) (* x1 y1)))))
            (else
             (cond   ((positive-pair? y0 y1)
                      (make-interval (* x0 y1) (* x1 y1)))
                     ((negative-pair? y0 y1)
                      (make-interval (* x1 y0) (* x0 y0)))
                     (else
                      (make-interval
                       ((lambda (a b) (if (< a b) a b)) (* x0 y1) (* x1 y0))
                       ((lambda (a b) (if (> a b) a b)) (* x0 y0) (* x1 y1)))))))))

(test old-mul-interval mul-interval)

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (percentage-of p n)
  (* (/ p 100.0) n))

; exercise 2.12

(println "***2.12")

(define (make-center-percent c p)
  (let ((w (percentage-of p c)))
    (make-interval (- c w) (+ c w))))

(define (percent z)
  (* (/ (width z) (center z)) 100))

(define a1 (make-center-percent 100 10))
(define a2 (make-center-percent 1000 23))

(width a1)
(percent a1)

(width a2)
(percent a2)

; exercise 2.13
; http://community.schemewiki.org/?sicp-ex-2.13

(define i1 (make-center-percent 100 0.05))
(define i2 (make-center-percent 100 0.04))
(define i3 (mul-interval i1 i2))

(percent i3)

; exercise 2.14

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one (add-interval (div-interval one r1)
                       (div-interval one r2)))))

(define aa (make-interval 2 8))
(define bb (make-interval 2 8))
(div-interval aa aa)
(div-interval aa bb)

; http://community.schemewiki.org/?sicp-ex-2.14-2.15-2.16
; http://wiki.drewhess.com/wiki/SICP_exercise_2.16

