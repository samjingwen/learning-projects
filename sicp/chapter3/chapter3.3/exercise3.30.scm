#lang sicp

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (ripple-carry-adder as bs ss c-out)

  (define (ripple-carry-iter as bs ss carry result)
    (cond ((null? as) 'ok)
          (else (full-adder (car as)
                            (car bs)
                            carry
                            (car ss)
                            (if (null? (cdr as))
                                c-out
                                result))
                (ripple-adder-iter (cdr as) (cdr bs) (cdr ss) result (make-wire)))))

  (let ((carry (make-wire))
        (sum (make-wire)))
    (ripple-carry-iter as bs ss carry sum)))





