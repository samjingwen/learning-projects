#lang sicp
(#%require (lib "27.ss" "srfi"))

; exercise 1.9
; recursive
; (define (+ a b)
;   (if (= a 0)
;       b
;       (inc (+ (dec a) b))))

; iterative
; (define (+ a b)
;   (if (= a 0) b (+ (dec a) (inc b))))

; exercise 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

(A 1 10)
(A 2 4)
(A 3 3)

(define (f n) (A 0 n)) ; 2n
(define (g n) (A 1 n)) ; 0 for n=0, 2^n for n>0
(define (h n) (A 2 n)) ; 0 for n=0, 2^n for n=1, 2^(2^(2^(2...(n times)))) for n>1
(define (k n) (* 5 n n)) ; 5n^2


; exercise 1.11
(define (func-recursive n)
  (cond
    ((< n 3) n)
    (else (+ (func-recursive (- n 1))
             (* 2 (func-recursive (- n 2)))
             (* 3 (func-recursive (- n 3)))))))

(func-recursive 3)

(define (func-iterative n)
  (define (func-iter a b c count)
    (if (= count 0)
        a
        (func-iter b c (+ c (* 2 b) (* 3 a)) (- count 1))))
  (func-iter 0 1 2 n))

(func-iterative 0)
(func-iterative 1)
(func-iterative 2)
(func-iterative 3)
(func-iterative 4)
(func-iterative 5)
(func-iterative 6)


; exercise 1.12
(define (pascal r c)
  (cond ((or (= c 1) (= c r)) 1)
        (else (+ (pascal (- r 1) (- c 1)) (pascal (- r 1) c)))))

(pascal 1 1) ; 1
(pascal 2 2) ; 1
(pascal 3 2) ; 2
(pascal 4 2) ; 3
(pascal 5 2) ; 4
(pascal 5 3) ; 6

; exercise 1.13
; http://community.schemewiki.org/?sicp-ex-1.13

; exercise 1.14
; http://community.schemewiki.org/?sicp-ex-1.14

; exercise 1.15
; a) 5 b) O(log(a))
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

(sine 12.15)

; exercise 1.16
(define (expt b n)
  (define (expt-iter b counter product)
    (if (= counter 0)
        product
        (expt-iter b
                   (- counter 1)
                   (* b product))))
  (expt-iter b n 1))


(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (square n)
  (* n n))

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt-iter b n)
  (define (go b n a)
    (cond ((= n 0) a)
          ((even? n) (go (square b) (/ n 2) a))
          (else (go b (- n 1) (* b a)))))
  (go b n 1))

(fast-expt-iter 5 0)
(fast-expt-iter 5 1)
(fast-expt-iter 5 2)
(fast-expt-iter 5 5)

; exercise 1.17
(define (double n) (* n 2))

(define (halve n) (/ n 2))

(define (times a b)
  (cond ((= b 0) 0)
        ((even? b) (double (times a (/ b 2))))
        (else (+ a (times a (- b 1))))))

(times 5 0)
(times 5 1)
(times 5 2)
(times 5 3)

; exercise 1.18
(define (fast-times a b)
  (define (go a b n)
    (cond ((= b 0) n)
          ((even? b) (go (double a) (halve b) n))
          (else (go a (- b 1) (+ a n)))))
  (go a b 0))

(fast-times 5 0)
(fast-times 5 1)
(fast-times 5 2)
(fast-times 5 3)

; exercise 1.19
; https://billthelizard.blogspot.com/2010/01/sicp-exercise-119-computing-fibonacci.html
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q)) ; compute p′
                   (+ (* 2 p q) (* q q)) ; compute q′
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)

; exercise 1.20
; normal order
; 1 + 2 + 4 + 7 + 4 = 18
; (gcd 206 40)
; if (= 40 0)
; (gcd 40 (rem 206 40))
; if (= (rem 206 40) 0) // 1
; if (= 6 0)
; (gcd (rem 206 40) (rem 40 (rem 206 40)))
; if (= (rem 40 (rem 206 40)) 0) // 2
; if (= 4 0)
; (gcd (rem 40 (rem 206 40)) (rem (rem 206 40) (rem 40 (rem 206 40))))
; if (= (rem (rem 206 40) (rem 40 (rem 206 40))) 0) // 4
; if (= 2 0)
; (gcd (rem (rem 206 40) (rem 40 (rem 206 40)))
;      (rem (rem 40 (rem 206 40)) (rem (rem 206 40) (rem 40 (rem 206 40)))))
; if (= (rem (rem 40 (rem 206 40)) (rem (rem 206 40) (rem 40 (rem 206 40)))) 0) // 7
; if (= 0 0)
; a -> (rem (rem 206 40) (rem 40 (rem 206 40))) // 4
;

; applicative order
; 1 + 1 + 1 + 1 = 4
; (gcd 206 40)
; if (= 40 0)
; (gcd 40 (rem 206 40)) ; 1
; (gcd 40 6)
; if (= 6 0)
; (gcd 6 (rem 40 6)) ; 1
; (gcd 6 4)
; if (= 4 0)
; (gcd 4 (rem 6 4)) ; 1
; (gcd 4 2)
; if (= 2 0)
; (gcd 2 (rem 4 2)) ; 1
; (gcd 2 0)
; 2

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(gcd 206 40)

;

(define (next n)
  (cond ((= n 2) 3)
        (else (+ n 2))))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b) (= (remainder b a) 0))

; (define (prime? n)
;   (= n (smallest-divisor n)))

(define (prime? n)
  (fast-prime? n 100))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random-integer (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

; exercise 1.21
(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

; exercise 1.22

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(timed-prime-test 4)

(define (search-for-primes start end)
  (define (iter n)
    (cond ((<= n end) (timed-prime-test n) (iter (+ n 2)))))
  (iter (if (odd? start) start (+ start 1))))

; 1e9, 721μs
(search-for-primes 1000000000 1000000021)
; 1e12, 17778μs
(search-for-primes 1000000000000 1000000000063)
(newline)

; exercise 1.23
(next 2)
(next 3)

; exercise 1.24
(timed-prime-test 1000037)

(timed-prime-test 1000000000061)
(timed-prime-test 1000000000063)

; exercise 1.25
; http://community.schemewiki.org/?sicp-ex-1.25

(expmod 5 101 101)

; exercise 1.26
; http://community.schemewiki.org/?sicp-ex-1.26


; exercise 1.27

; 561, 1105, 1729, 2465, 2821, and 6601
(define (full-fermat-test n)
  (define (go a)
    (cond
      ((> a n) true)
      ((and (< a n) (= (expmod a n n) a)) (go (+ a 1)))
      (else false)))
  (go 1))


(fermat-test 561)
(full-fermat-test 561)
(fermat-test 1105)
(full-fermat-test 1105)
(fermat-test 1729)
(full-fermat-test 1729)
(fermat-test 2465)
(full-fermat-test 2465)
(fermat-test 2821)
(full-fermat-test 2821)
(fermat-test 6601)
(full-fermat-test 6601)

; exercise 1.28
; http://community.schemewiki.org/?sicp-ex-1.28

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

(define (fast-prime?? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (prime?? n)
  (fast-prime?? n 100))

(newline)
(fermat-test 561)
(prime?? 561)






