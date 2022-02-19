#lang sicp

(define (display-stream-until s n)
  (if (> n 0)
      (begin (newline)
             (display (stream-car s))
             (display-stream-until (stream-cdr s)
                                   (- n 1)))))

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

(define (add-streams s1 s2) (stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers-starting-from-2 (integers-starting-from 2))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2)
                                          (stream-car s1))
                            (mul-series (stream-cdr s1)
                                        s2))))

(define (invert-unit-series s)
  (define iter
    (cons-stream 1
                 (mul-series (stream-map -
                                         (stream-cdr s))
                             (invert-unit-series s))))
  iter)

(define (integrate-series s)
  (cons-stream (stream-car s)
               (stream-map / (stream-cdr s) integers-starting-from-2)))
(define cosine-series
  (cons-stream 1 (stream-map - (integrate-series sine-series))))
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(define (div-series s1 s2)
  (mul-series s1 (invert-unit-series s2)))

(define tangent-series (div-series sine-series cosine-series))

; https://socratic.org/questions/what-is-the-taylor-series-expansion-for-the-tangent-function-tanx
(display-stream-until tangent-series 10)


