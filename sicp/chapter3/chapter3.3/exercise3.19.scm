#lang sicp

; input will always be pair
(define (cycle? x)
  (let ((head x))
    (define (iter lst)
      (cond ((null? lst) #f)
            ((eq? head lst) #t)
            (else (iter (cdr lst)))))
    (iter (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define z (make-cycle (list 'a 'b 'c)))

(cycle? z)
(cycle? (list 'a 'b 'c))
(cycle? (list 'a 'b 'c 'a))



