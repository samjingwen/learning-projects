#lang sicp

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (sign-change-detector x last-value)
  (let ((sign (* x last-value)))
    (if (< sign 0)
        (if (< last-value 0)
            1
            -1)
        0)))

(define (fold-right op initial sequence)
  (define (iter acc rest)
    (if (null? rest)
        acc
        (op (car rest) (iter acc (cdr rest)))))
  (iter initial sequence))

(define (list->stream xs)
  (fold-right (lambda (x ys) (cons-stream x ys))
              the-empty-stream
              xs))

(define sense-data
  (list->stream (list 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)))

(define (average x y) (/ (+ x y) 2))

(define (smooth input-stream)
  (define (iter initial rest)
    (cond ((stream-null? rest) the-empty-stream)
          (else (cons-stream (average initial (stream-car rest))
                             (iter (stream-car rest) (stream-cdr rest))))))
  (iter 0 input-stream))

(define zero-crossings
  (stream-map sign-change-detector
              (smooth sense-data)
              (cons-stream 0 (smooth sense-data))))

(define (display-line x) (newline) (display x))

(define (display-stream s)
  (stream-for-each display-line s))

(define sss zero-crossings)

(display-stream sss)
