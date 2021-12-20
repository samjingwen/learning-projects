#lang sicp

(define (make-account balance password)
  (let ((count 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define call-the-cops "call the cops!")
    (define (dispatch check message)
      (if (eq? check password)
          (cond ((eq? message 'withdraw) withdraw)
                ((eq? message 'deposit) deposit)
                (else (error "Unknown request: MAKE-ACCOUNT"
                             message)))
          (lambda (x)
            (begin (set! count (+ count 1))
                   (if (> count 7)
                       call-the-cops
                       "Incorrect password")))))
    dispatch))

(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'withdraw) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'withdraw) 50)
((acc 'some-other-password 'withdraw) 50)
((acc 'some-other-password 'withdraw) 50)
((acc 'some-other-password 'withdraw) 50)







