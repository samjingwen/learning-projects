#lang sicp

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell) (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell) true (begin (set-car! cell true) false)))

(define (make-semaphore-with-mutex n)
  (let ((cell (list 0))
        (lock (make-mutex)))

    (define (test-and-set! cell)
      (lock 'acquire)
      (cond ((= (car cell) n)
             (lock 'release)
             true)
            (else (set-car! cell (+ 1 (car cell)))
                  (lock 'release)
                  false)))

    (define (clear! cell)
      (lock 'acquire)
      (set-car! cell (- 1 (car cell)))
      (lock 'release))

    (define (the-semaphore m)
      (cond ((eq? m 'acqure)
             (if (test-and-set! cell)
                 (the-semaphore 'acqure)))
            ((eq? m 'release) (clear! cell))))
    the-semaphore))

(define (make-semaphore-with-test-and-set n)
  (let ((count 0)
        (cell (list false)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-semaphore 'acquire)
                 (if (< count n)
                     (begin (set! count (+ count 1))
                            (clear! cell))
                     (the-semaphore 'acquire))))
            ((eq? m 'release)
             (if (test-and-set! cell)
                 (the-semaphore 'acquire)
                 (begin (set! count (- count 1))
                        (clear! cell))))))
    the-semaphore))

