#lang sicp


(define rand 
  (let ((init 0))
    (define (dispatch op)
      (cond ((eq? 'generate op)
             (begin (set! init (rand-update init))
                    init))
            ((eq? 'reset op)
             (lambda (x) (begin (set! init x) x)))))
    dispatch))


(define (rand-update x)
  (+ x 1))

(rand 'generate)
(rand 'generate)
(rand 'generate)
((rand 'reset) 88)
(rand 'generate)
(rand 'generate)
(rand 'generate)

