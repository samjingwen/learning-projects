#lang sicp

(define (or-gate a1 a2 output)
  (let ((c (make-wire))
        (d (make-wire))
        (e (make-wire)))
    (inverter a1 c)
    (inverter a2 d)
    (and-gate c d e)
    (inverter e output)
    'ok))

; one and-gate-delay and two inverter-delay



