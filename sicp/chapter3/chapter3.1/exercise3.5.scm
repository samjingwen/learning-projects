#lang sicp

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random (+ range 1)))))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (* (monte-carlo trials (lambda ()
                           (let ((x (random-in-range x1 x2))
                                 (y (random-in-range y1 y2)))
                             (p x y))))
     (* (- x2 x1)
        (- y2 y1))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

(define (circle-test? x y)
  (<= (+ (expt (- x 0) 2)
         (expt (- y 0) 2))
      (expt 1000 2)))

(define estimate-pi
  (exact->inexact
   (/ (estimate-integral circle-test? -1000 1000 -1000 1000 10000)
      (expt 1000 2))))

estimate-pi
