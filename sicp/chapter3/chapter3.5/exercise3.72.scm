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

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x) (newline) (display x))

(define (display-stream-until s n)
  (if (> n 0)
      (begin (newline)
             (display (stream-car s))
             (display-stream-until (stream-cdr s)
                                   (- n 1)))))

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else (let ((s1car (stream-car s1))
                    (s2car (stream-car s2)))
                (cond ((> (weight s1car) (weight s2car))
                       (cons-stream s2car
                                    (merge-weighted s1 (stream-cdr s2) weight)))
                      (else
                       (cons-stream s1car
                                    (merge-weighted (stream-cdr s1) s2 weight))))))))

(define (pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

(define (square x) (* x x))

(define (sum-of-squares x)
  (+ (square (car x)) (square (cadr x))))

(define squared-pairs
  (pairs integers integers sum-of-squares))

(define (sum-of-squares-stream)
  (define (iter pairs-stream)
    (let ((next-1 (stream-cdr pairs-stream))
          (next-2 (stream-cdr (stream-cdr pairs-stream))))
      (let ((p1 (stream-car pairs-stream))
            (p2 (stream-car next-1))
            (p3 (stream-car next-2)))
        (let ((x1 (sum-of-squares p1))
              (x2 (sum-of-squares p2))
              (x3 (sum-of-squares p3)))
          (if (= x1 x2 x3)
              (cons-stream (list x1 p1 p2 p3)
                           (iter next-2))
              (iter next-1))))))

  (iter squared-pairs))

(display-stream-until (sum-of-squares-stream) 10)

; TODO: use a general accum/reduce procedure
