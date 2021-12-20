#lang sicp

(define (make-monitored f)
  (let ((count 0))
    (lambda (x)
      (cond ((eq? 'how-many-calls? x) count)
            ((eq? 'reset-count x) (begin (set! count 0)
                                         count))
            (else (begin (set! count (+ count 1)) (f x)))))))

(define (square x) (* x x))

(define mf (make-monitored square))

(mf 10)
(mf 4)
(mf 6)
(mf 'how-many-calls?)
(mf 'reset-count)
(mf 8)
(mf 5)
(mf 9)
(mf 9)
(mf 'how-many-calls?)


