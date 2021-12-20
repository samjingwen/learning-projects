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

  (define (incorrect-password x) "Incorrect password")

  (define (create-joint-acct account joint-acct-password)
    (define (joint-dispatch check message)
      (if (eq? check joint-acct-password)
          (dispatch password message)
          incorrect-password))
    joint-dispatch)

  (define (dispatch check message)
    (if (eq? check password)
        (cond ((eq? message 'withdraw) withdraw)
              ((eq? message 'deposit) deposit)
              ((eq? message 'create-joint-acct) create-joint-acct)
              (else (error "Unknown request: MAKE-ACCOUNT"
                           message)))
        incorrect-password))

  dispatch)

(define (make-joint account password joint-acct-password)
  ((account password 'create-joint-acct) account
                                         joint-acct-password))

(define paul-acc (make-account 100 'paul-secret))
((paul-acc 'paul-secret 'withdraw) 40)
((paul-acc 'wrong-password 'deposit) 50)

(define peter-acc (make-joint paul-acc 'paul-secret 'peter-secret))

((peter-acc 'peter-secret 'withdraw) 20)
((peter-acc 'peter-secret 'deposit) 20)
((peter-acc 'wrong-password 'deposit) 20)
((peter-acc 'paul-secret 'deposit) 20)

(define john-acc (make-joint peter-acc 'peter-secret 'john-secret))
((john-acc 'john-secret 'withdraw) 20)
((john-acc 'john-secret 'deposit) 50)
((john-acc 'paul-secret 'deposit) 50)
((john-acc 'peter-secret 'deposit) 50)


