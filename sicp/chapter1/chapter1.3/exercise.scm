#lang sicp
(#%require (lib "27.ss" "srfi"))


(define (println output)
  (display output)
  (newline))

(define (cube n) (* n n n))

(define (square x) (* x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(sum-cubes 1 10)

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

(sum-integers 1 10)

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next a)
    (+ a 4))
  (sum pi-term a pi-next b))

(pi-sum 1 5)

(* 8 (pi-sum 1 1000))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(integral cube 0 1 0.01)


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

; exercise 1.29

(define (simpson-rule f a b n)
  (define h
    (/ (- b a) n))

  (define (y k)
    (f (+ a (* k h))))

  (define (simpson-term x)
    (cond ((or (= x 0) (= x n)) (y x))
          ((even? x) (* 2 (y x)))
          (else (* 4 (y x)))))

  (* (/ h 3) (sum simpson-term 0 inc n)))


(simpson-rule cube 0 1 100)
(simpson-rule cube 0 1 1000)

; exercise 1.30

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (sum-integers-iter a b)
  (sum-iter identity a inc b))

(sum-integers-iter 1 10)

; exercise 1.31
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (product-integers a b)
  (product identity a inc b))

(product-integers 1 5)

(define (factorial n)
  (product-integers 1 n))

(factorial 0)
(factorial 1)
(factorial 2)
(factorial 3)
(factorial 4)
(factorial 5)

(define (wallis-pi n)
  (define (pi-term n)
    (if (even? n)
        (/ (+ n 2) (+ n 1))
        (/ (+ n 1) (+ n 2))))
  (product pi-term 1 inc n))

(* (wallis-pi 100) 4.0)
(* (wallis-pi 1000) 4.0)

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(product-iter identity 1 inc 5)
(product-iter identity 1 inc 6)
(product-iter identity 1 inc 7)

; exercise 1.32

(define (accumulate combiner acc term a next b)
  (if (> a b)
      acc
      (combiner
       (term a)
       (accumulate combiner acc term (next a) next b))))

(accumulate + 0 identity 1 inc 10)
(accumulate * 1 identity 1 inc 10)

(define (accumulate-iter combiner acc term a next b)
  (if (> a b)
      acc
      (accumulate-iter combiner
                       (combiner acc (term a))
                       term
                       (next a)
                       next b)))

(accumulate-iter + 0 identity 1 inc 10)
(accumulate-iter * 1 identity 1 inc 10)

; exercise 1.33

(define (filtered-accumulate combiner acc term a next b filter?)
  (if (> a b)
      acc
      (cond
        ((filter? a)
         (filtered-accumulate combiner
                              (combiner acc (term a))
                              term
                              (next a)
                              next
                              b
                              filter?))
        (else
         (filtered-accumulate combiner
                              acc
                              term
                              (next a)
                              next
                              b
                              filter?)))))

(filtered-accumulate + 0 identity 5 inc 10 prime?)


(define (relatively-prime-product n)
  (define (gcd a b)
    (if (= b 0) a (gcd b (remainder a b))))

  (define (relatively-prime? a)
    (= 1 (gcd a n)))

  (filtered-accumulate * 1 identity 1 inc n relatively-prime?))

(relatively-prime-product 10)

; exercise 1.34

(define (f g) (g 2))

; (f f)
; error, expected a procedure that can be applied to arguments

(define (average x y)
  (/ (+ x y) 2))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (close-enough? x y) (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

(half-interval-method sin 2.0 4.0)

(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
                      1.0
                      2.0)

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))

  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))

  (try first-guess))

; (fixed-point cos 1.0)

; (fixed-point (lambda (y) (+ (sin y) (cos y)))
;              1.0)

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp
                (lambda (y)
                  (/ x (square y))))))

; exercise 1.35

; (fixed-point (lambda (x) (+ 1 (/ 1 x)))
;              1.0)

; exercise 1.36
; 34 iterations
; 11 iterations

; (fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
;              10.0)

; exercise 1.37

(define golden-ratio 1.618033988749894)

(define (cont-frac n d k)
  (define (go i)
    (if (> i k)
        0
        (/ (n i)
           (+ (d i)
              (go (+ i 1))))))
  (go 1))

(define (cont-frac-iter n d k)
  (define (go i res)
    (if (= i 0)
        res
        (go (- i 1)
            (/ (n i) (+ (d i) res)))))
  (go k 0.0))

(println "***1.37")

(/ 1
   (cont-frac (lambda (i) 1.0)
              (lambda (i) 1.0)
              10))

(/ 1
   (cont-frac-iter (lambda (i) 1.0)
                   (lambda (i) 1.0)
                   10))

; exercise 1.38

(println "***1.38")

(define (e-euler k)
  (+ 2.0 (cont-frac (lambda (i) 1.0)
                    (lambda (i)
                      (if (= (remainder i 3) 2.0)
                          (/ (+ i 1) 1.5)
                          1))
                    k)))

(define (e-euler-iter k)
  (+ 2.0 (cont-frac-iter (lambda (i) 1.0)
                         (lambda (i) (if (= (remainder i 3) 2.0)
                                         (/ (+ i 1) 1.5)
                                         1))
                         k)))

(e-euler 100)
(e-euler-iter 100)

; exercise 1.39

(println "***1.39")

(define (tan-cf x k)
  (let ((a (- (* x x))))
    (cont-frac (lambda (i) (if (= i 1) x a))
               (lambda (i) (- (* i 2) 1.0))
               k)))

(define (tan-cf-iter x k)
  (let ((a (- (* x x))))
    (cont-frac-iter (lambda (i) (if (= i 1) x a))
                    (lambda (i) (- (* i 2) 1.0))
                    k)))

(tan-cf 10 1000)
(tan-cf-iter 10 1000)

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define dx 0.00001)

; ((deriv cube) 5)

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

; exercise 1.40

(println "***1.40")

(define (cubic a b c)
  (lambda (x) (+ (cube x)
                 (* a (square x))
                 (* b x)
                 c)))

(newtons-method (cubic 1 1 1) 1)

; exercise 1.41

(println "***1.41")

(define (double f) (lambda (x) (f (f x))))

((double inc) 5)

; exercise 1.42

(println "***1.42")

(define (compose f g) (lambda (x) (f (g x))))

((compose square inc) 6)
((compose inc square) 6)

; exercise 1.43

(println "***1.43")

(define (repeated f times)
  (define (go ff i)
    (if (= i times)
        ff
        (go (compose f ff) (+ 1 i))))
  (go f 1))

((repeated square 1) 5)
((repeated square 2) 5)
((repeated square 3) 5)
((repeated square 4) 5)

; exercise 1.44

(println "***1.44")

(define (smooth f)

  (define (average a b c)
    (/ (+ a b c) 3))

  (lambda (x) (average
               (f (- x dx))
               (f x)
               (f (+ x dx)))))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

; exercise 1.45

(println "***1.45")

(define (get-max-power n)
  (define (go p r)
    (if (< n r)
        (- p 1)
        (go (+ p 1) (* r 2))))
  (go 1 2))

(get-max-power 4)

(define (power b p)
  (define (go res a n)
    (if (= n 0)
        res
        (if (even? n)
            (go res (square a) (/ n 2))
            (go (* res a) a (- n 1)))))
  (go 1 b p))

(power 2 3)

(define (nth-root n x)
  (define (nth-average-damp times)
    (repeated average-damp times))

  (fixed-point ((nth-average-damp n)
                (lambda (y) (/ x
                               (power y
                                      (- n 1)))))
               1.0))

(nth-root 2 10)
(nth-root 3 10)
(nth-root 4 10)
(nth-root 5 10)
(nth-root 6 10)

; exercise 1.46

(println "***1.46")

(define (iterative-improve good-enough? improve-guess)
  (define (go guess)
    (if (good-enough? guess)
        guess
        (go (improve-guess guess))))
  (lambda (guess) (go guess)))

(define (sqrt-iter guess x)
  (define (improve guess)
    (average guess (/ x guess)))

  (define (average a b)
    (/ (+ a b) 2))

  (define (good-enough? guess)
    (and
     (< (/ x (square guess)) 1.0001)
     (> (/ x (square guess)) 0.9999)))

  ((iterative-improve good-enough? improve) guess))

(sqrt-iter 1.0 9)
(sqrt-iter 1.0 81)

(define (fixed-point-iter f first-guess)
  (define (close-enough? x)
    (< (abs (- x (f x)))
       tolerance))

  ((iterative-improve close-enough? f) first-guess))

(define (sqrt-iter-improve x)
  (fixed-point-iter (lambda (y) (average y (/ x y)))
                    1.0))

(define (cube-root-iter x)
  (fixed-point-iter (average-damp (lambda (y)
                                    (/ x (square y))))
                    1.0))

(sqrt-iter-improve 9)
(sqrt-iter-improve 81)

(cube-root-iter 8)
(cube-root-iter 64)






