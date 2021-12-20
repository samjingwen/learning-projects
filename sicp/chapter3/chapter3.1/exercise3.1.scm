#lang sicp

(define (make-accumulator n)
    (lambda (x) (begin (set! n (+ x n)) n)))

(define A (make-accumulator 5))

(A 10)
(A 10)
(A 15)

