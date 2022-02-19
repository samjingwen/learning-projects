#lang sicp

(define (cons-stream a b) (cons a (delay b)))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
(define stream-null? null?)
(define the-empty-stream '())

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

(define (display-line x) (newline) (display x))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-stream-until s n)
  (if (> n 0)
      (begin (newline)
             (display (stream-car s))
             (display-stream-until (stream-cdr s)
                                   (- n 1)))
      'done))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define (random-number-generator command-stream)
  (define random-number
    (cons-stream random-init
                 (stream-map (lambda (number command)
                               (cond ((null? command) the-empty-stream)
                                     ((eq? command 'generator)
                                      (random-update number))
                                     ((and (pair? command)
                                           (eq? (car command) 'reset))
                                      (cdr command))
                                     (else
                                      (error "bad command -- " command))))
                             random-number
                             command-stream)))
  random-number)

(define (random-update x)
  (remainder (+ (* 13 x) 5) 24))

(define random-init (random-update (expt 2 32)))

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

(define s1 (random-number-generator (list '(reset 2010)
                                          '(generate)
                                          '(generate)
                                          '(generate)
                                          '(reset 2020)
                                          '(generate)
                                          '(generate)
                                          '(reset 1234)
                                          '(generate)
                                          '(generate))))

(display-stream s1)

