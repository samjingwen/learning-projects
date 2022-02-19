#lang sicp

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))


(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(define a (expand 1 7 10))
(define b (expand 3 8 10))

(stream-ref a 0)
(stream-ref a 1)
(stream-ref a 2)
(stream-ref a 3)
(stream-ref a 4)

(stream-ref b 0)
(stream-ref b 1)
(stream-ref b 2)
(stream-ref b 3)
(stream-ref b 4)


