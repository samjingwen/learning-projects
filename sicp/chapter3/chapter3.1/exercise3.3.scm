#lang sicp

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch check message)
    (if (eq? check password)
        (cond ((eq? message 'withdraw) withdraw)
              ((eq? message 'deposit) deposit)
              (else (error "Unknown request: MAKE-ACCOUNT"
                           message)))
        (lambda (x) "Incorrect password")))
  dispatch)

(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)







