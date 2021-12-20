#lang sicp

(define f
  (let ((n 10))
    (lambda (x) (begin (set! n (- x n))
                       (+ x n)))))

; (+ (f 0) (f 1))

(+ (f 1) (f 0))


