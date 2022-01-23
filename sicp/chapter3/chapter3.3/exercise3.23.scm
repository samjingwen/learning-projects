#lang sicp

(define (make-deque)
  (let ((front-ptr '())
        (rear-ptr '()))

    (define (print-deque)
      (define (iter ptr)
        (cond ((null? ptr) (display '()) (newline))
              (else (display (car ptr))
                    (display " ")
                    (iter (caddr ptr)))))
      (iter front-ptr))

    (define (empty-deque?)
      (null? front-ptr))

    (define (front-deque)
      (if (empty-deque?)
          (error "FRONT called with an empty queue" front-ptr)
          (car front-ptr)))

    (define (rear-deque)
      (if (empty-deque?)
          (error "REAR called with an empty queue" rear-ptr)
          (car rear-ptr)))

    (define (front-insert-deque! item)
      (let ((new-pair (list item '() '())))
        (cond ((empty-deque?) (set! front-ptr new-pair)
                              (set! rear-ptr new-pair))
              (else (set-car! (cddr new-pair) front-ptr)
                    (set-car! (cdr front-ptr) new-pair)
                    (set! front-ptr new-pair)))))

    (define (rear-insert-deque! item)
      (let ((new-pair (list item '() '())))
        (cond ((empty-deque?) (set! front-ptr new-pair)
                              (set! rear-ptr new-pair))
              (else (set-car! (cdr new-pair) rear-ptr)
                    (set-car! (cddr rear-ptr) new-pair)
                    (set! rear-ptr new-pair)))))

    (define (front-delete-deque!)
      (cond ((empty-deque?) (error "DELETE! called with an empty queue" front-ptr))
            (else (set! front-ptr (caddr front-ptr))
                  (if (null? front-ptr)
                      (set! rear-ptr '())
                      (set-car! (cdr front-ptr) '())))))

    (define (rear-delete-deque!)
      (cond ((empty-deque?) (error "DELETE! called with an empty queue" front-ptr))
            (else (set! rear-ptr (cadr rear-ptr))
                  (if (null? rear-ptr)
                      (set! front-ptr '())
                      (set-car! (cddr rear-ptr) '())))))

    (define (dispatch m)
      (cond ((eq? m 'empty-deque?) empty-deque?)
            ((eq? m 'front-deque) front-deque)
            ((eq? m 'rear-deque) rear-deque)
            ((eq? m 'front-insert-deque!) front-insert-deque!)
            ((eq? m 'rear-insert-deque!) rear-insert-deque!)
            ((eq? m 'front-delete-deque!) front-delete-deque!)
            ((eq? m 'rear-delete-deque!) rear-delete-deque!)
            ((eq? m 'print-deque) print-deque)
            (else (error "Unknown operation" m))))

    dispatch))

(define (empty-deque? deque)
  ((deque 'empty-deque?)))
(define (front-deque deque)
  ((deque 'front-deque)))
(define (rear-deque deque)
  ((deque 'rear-deque)))
(define (front-insert-deque! deque item)
  ((deque 'front-insert-deque!) item))
(define (rear-insert-deque! deque item)
  ((deque 'rear-insert-deque!) item))
(define (front-delete-deque! deque)
  ((deque 'front-delete-deque!)))
(define (rear-delete-deque! deque)
  ((deque 'rear-delete-deque!)))
(define (print-deque deque)
  ((deque 'print-deque)))


(define q1 (make-deque))

(front-insert-deque! q1 'a)
(front-insert-deque! q1 'b)
(front-insert-deque! q1 'c)

(rear-insert-deque! q1 'd)
(rear-insert-deque! q1 'e)
(rear-insert-deque! q1 'f)

(print-deque q1)

(front-delete-deque! q1)
(rear-delete-deque! q1)

(print-deque q1)

(front-delete-deque! q1)
(rear-delete-deque! q1)

(print-deque q1)

(front-delete-deque! q1)

(print-deque q1)
(front-delete-deque! q1)

(print-deque q1)

