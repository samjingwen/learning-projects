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

(define (add-streams s1 s2) (stream-map + s1 s2))

(define (partial-sums s)
  (define iter (add-streams s (cons-stream 0 iter)))
  iter)

(define (stream-limit s tolerance)
  (define (iter prev s)
    (if (< (abs (- prev (stream-car s))) tolerance)
        (stream-car s)
        (iter (stream-car s) (stream-cdr s))))
  (iter (stream-car s) (stream-cdr s)))

(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream (partial-sums (ln2-summands 1)))

(define (square n) (* n n))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x) (newline) (display x))

; (display-stream ln2-stream)
; (display-stream (euler-transform ln2-stream))
; (display-stream
;  (accelerated-sequence euler-transform ln2-stream))