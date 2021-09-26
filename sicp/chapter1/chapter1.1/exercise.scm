#lang sicp

;; exercise 1.1

10 ;; 10
(+ 5 3 4) ;; 12
(- 9 1) ;; 8
(/ 6 2) ;; 3
(+ (* 2 4) (- 4 6)) ;; 6
(define a 3) 
(define b (+ a 1))
(+ a b (* a b)) ;; 19
(= a b) ;; #f
(if (and (> b a) (< b (* a b)))
    b
    a) ;; 4
(cond 
    ((= a 4) 6)
    ((= b 4) (+ 6 7 a))
    (else 25)) ;; 16
(+ 2 (if (> b a) b a)) ;; 6
(*  (cond 
        ((> a b) a) 
        ((< a b) b)
        (else -1))
    (+ a 1)) ;; 16

;; exercise 1.2

(/  (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) 
    (* 3 (- 6 2) (- 2 7)))

;; exercise 1.3

(define (squareOfTwoLargest a b c) 
        (cond 
            ((and (< a b) (< a c)) (+ (* b b) (* c c)))
            ((and (< b a) (< b c)) (+ (* a a) (* c c)))
            (else (+ (* a a) (* b b)))))

;; exercise 1.4
;; if b > 0 then a + b else a - b

;; exercise 1.5
;; applicative order would not terminate, operators and operands are evaluated and then applied
;; normal order would evaluate to 0
;; https://stackoverflow.com/questions/16036139/seek-for-some-explanation-on-sicp-exercise-1-5

(define (square x) (* x x))

(define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))

(define (improve guess x)
    (average guess (/ x guess)))

(define (average x y)
    (/ (+ x y) 2))

(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
    (sqrt-iter 1.0 x))

(sqrt 9)
(sqrt (+ 100 37))
(sqrt (+ (sqrt 2) (sqrt 3)))
(square (sqrt 1000))

;; exercise 1.6
;; new-if will keep calling itself due to the third parameter in new-sqrt-iter
;; https://stackoverflow.com/questions/1171252/whats-the-explanation-for-exercise-1-6-in-sicp

(define (new-if predicate then-clause else-clause)
    (cond (predicate then-clause)
        (else else-clause)))

(new-if (= 2 3) 0 5)

(new-if (= 1 1) 0 5)

(define (new-sqrt-iter guess x)
    (new-if (good-enough? guess x)
        guess
        (new-sqrt-iter (improve guess x) x)))

;; (new-sqrt-iter 1 2)

;; exercise 1.7 
;; http://community.schemewiki.org/?sicp-ex-1.7
;; check for relative tolerance of guess within 1.0001 and 0.9999 or higher precision:
(define (good-enough2? guess x) 
    (and 
        (< (/ x (square guess)) 1.0001) 
        (> (/ x (square guess)) 0.9999)))

(define (sqrt-iter2 guess x)
    (if (good-enough2? guess x)
        guess
        (sqrt-iter2 (improve guess x) x)))

(define (sqrt2 x)
    (sqrt-iter2 1.0 x))


(good-enough2? 5 10)

(sqrt 0.0001)

(sqrt2 0.0001)


;; exercise 1.8
(define (cube x)(* x x x))

(define (cube-enough? guess x)
    (and 
        (< (/ x (cube guess)) 1.0001)
        (> (/ x (cube guess)) 0.9999)))

(define (cuberoot-iter guess x)
    (if (cube-enough? guess x) 
        guess
        (cuberoot-iter (improve-cube-guess guess x) x)))

(define (improve-cube-guess guess x)
    (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (cuberoot x)
    (cuberoot-iter 1.0 x))

(cuberoot 512)