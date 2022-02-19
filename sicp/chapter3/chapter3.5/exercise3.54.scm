#lang sicp

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (mul-streams . argstreams)
  (if (stream-null? argstreams)
      the-empty-stream
      (cons-stream
       (apply * (map stream-car argstreams))
       (apply mul-streams (map stream-cdr argstreams)))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers-from-2 (integers-starting-from 2))

(define factorials
  (cons-stream 1 (mul-streams factorials integers-from-2)))

(stream-ref factorials 0)
(stream-ref factorials 1)
(stream-ref factorials 2)
(stream-ref factorials 3)
(stream-ref factorials 4)
(stream-ref factorials 5)

