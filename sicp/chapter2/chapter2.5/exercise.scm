#lang sicp

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))

  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))

  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)

  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))


; exercise 2.77
; method not installed in lookup table
; apply generic called twice
; 'magnitude 'complex
; 'magnitude 'rectangular

; exercise 2.78

(define (attach-tag type-tag contents)
  (if (eq? 'scheme-number type-tag)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) datum)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum: TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (error "Bad tagged datum: CONTENTS" datum)))

; exercise 2.79
; exercise 2.80

(define (install-generic-aritmetic-package)
  (define tolerance 0.0001)

  (define (=zero? x)
    (and (<= (abs (real-part x)) tolerance)
         (<= (abs (imag-part x)) tolerance)))

  (define (equ? a b)
    (zero? (sub a b)))

  (put 'real-part '(scheme-number) (lambda (x) x))
  (put 'imag-part '(scheme-number) (lambda (x) 0))
  (put 'real-part '(rational) (lambda (x) (/ (numer x) (denom x))))
  (put 'imag-part '(rational) (lambda (x) 0))

  (put 'equ? 'generic equ?)


  (put '=zero? '(rectangular) (lambda (x) (and (=zero? (real-part x))
                                               (=zero? (imag-part x)))))

  (put '=zero? '(polar) (lambda (x) (=zero? (magnitude x))))

  (put '=zero? '(complex) =zero?)

  'done)

(define (equ? a b)
  ((get 'equ? 'generic) a b))

(define (=zero? data)
  (apply-generic '=zero? data))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else (error "No method for these types"
                                     (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

; exercise 2.81
; a - infinite loop
; b - works correctly if procedure is installed

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (cond ((eq? type1 type2) (error "No method for these types"
                                                (list op type-tags)))
                      (else (let ((t1->t2 (get-coercion type1 type2))
                                  (t2->t1 (get-coercion type2 type1)))
                              (cond (t1->t2
                                     (apply-generic op (t1->t2 a1) a2))
                                    (t2->t1
                                     (apply-generic op a1 (t2->t1 a2)))
                                    (else (error "No method for these types"
                                                 (list op type-tags))))))))
              (error "No method for these types"
                     (list op type-tags)))))))

; exercise 2.82

(define (apply-generic op . args)
  (define (type-tags args)
    (map type-tag args))

  (define (try-coerce-to target)
    (map (lambda (x)
           (let (coercing (get-coercion (type-tag x) (type-tag target)))
             (if coercing
                 (coercing x)
                 x)))
         args))

  (define (iter next)
    (if (null? next)
        (error "No coercion methods found for these types " (list op (type-tags args)))
        (let ((coerced (try-coerce-to (car next))))
          (let ((proc (get op coerced)))
            (if proc
                (apply proc (map contents coerced))
                (iter (cdr next)))))))

  (let ((proc (get op (type-tags args))))
    (if proc
        (apply proc (map contents args))
        (iter args))))

; Situation where this is not sufficiently general:
; types: A B C
; registered op: (op some-A some-B some-B)
; registered coercion: A->B C->B
; Situation: Evaluating (apply-generic op A B C) will only try (op A B C), (op B B B) and fail
; while we can just coerce C to B to evaluate (op A B B) instead

; exercise 2.83

(define (install-raise-operation)
  (define (integer->rational x)
    (make-rational x 1))
  (define (rational->real x)
    (make-real (/ (numer x) (denom x))))
  (define (real->complex x)
    (make-complex-from-real-imag x 0))

  (put-coercion 'raise 'integer integer->rational)
  (put-coercion 'raise 'rational rational->real)
  (put-coercion 'raise 'real real->complex)
  'done)

(define (raise x)
  (apply-generic 'raise x))

; exercise 2.84

(define (install-type-level)
  (put 'type-level '(integer) (lambda (x) 1))
  (put 'type-level '(rational) (lambda (x) 2))
  (put 'type-level '(real) (lambda (x) 3))
  (put 'type-level '(complex) (lambda (x) 4))
  'done)

(define (type-level x) (apply-generic 'type-level x))

(define (apply-generic op . args)
  (define (type-tags args)
    (map type-tag args))

  (define (find-highest-type-level args)
    (define (iter result next)
      (if (null? next)
          result
          (if (> (type-level (car next)) result)
              (iter (type-level (car next)) (cdr args))
              (iter result (cdr args)))))
    (iter 0 args))

  (define (raise-to type target)
    (cond ((eq? (type-tag type) (type-tag target)) type)
          ((< (type-tag type) (type-tage target)) (raise-to (raise type) target))
          (else (error "cannot raise type to a lower target"))))

  (define (apply-with-raised-types args)
    (let ((target-type-level (find-highest-type-level args)))
      (let ((raised-args (map (lambda (x) (raise-to x target-type-level))
                              args)))
        (let ((proc (get op (type-tags raised-args))))
          (if proc
              (apply proc (map contents args))
              (error "no method for these types" (list op (type-tags raised-args))))))))

  (let ((proc (get op (type-tags args))))
    (if proc
        (apply proc (map contents args))
        (apply-with-raised-types args))))

; exercise 2.85

(define (apply-generic op . args)
  (define (reduce-type x)
    (cond ((eq? op 'add) (drop x))
          ((eq? op 'sub) (drop x))
          ((eq? op 'mul) (drop x))
          ((eq? op 'div) (drop x))
          (else x)))

  (define (type-tags args)
    (map type-tag args))

  (define (find-highest-type-level args)
    (define (iter result next)
      (if (null? next)
          result
          (if (> (type-level (car next)) result)
              (iter (type-level (car next)) (cdr args))
              (iter result (cdr args)))))
    (iter 0 args))

  (define (raise-to type target)
    (cond ((eq? (type-tag type) (type-tag target)) type)
          ((< (type-tag type) (type-tage target)) (raise-to (raise type) target))
          (else (error "cannot raise type to a lower target"))))

  (define (apply-with-raised-types args)
    (let ((target-type-level (find-highest-type-level args)))
      (let ((raised-args (map (lambda (x) (raise-to x target-type-level))
                              args)))
        (let ((proc (get op (type-tags raised-args))))
          (if proc
              (apply proc (map contents args))
              (error "no method for these types" (list op (type-tags raised-args))))))))

  (let ((proc (get op (type-tags args))))
    (if proc
        (reduce-type (apply proc (map contents args)))
        (apply-with-raised-types args))))

(define (project z) (apply-generic 'project z))

(define (drop x)
  (if (= (type-level x) 1)
      x
      (let ((projected (project x)))
        (if (equ? x (raise projected))
            (drop projected)
            x))))

(define (install-project-rational-package)
  (define (project r)
    (make-integer (truncate (/ (numer r) (denom r)))))

  (put 'project '(rational) (lambda (x) (project x)))
  'done)

(define (install-project-real-package)
  (define (project r)
    (let ((exact (inexact->exact r)))
      (cond ((integer? exact)  (make-rational exact 1))
            ((rational? exact) (make-rational (numerator exact) (denominator exact)))
            (else (make-rational (truncate exact) 1)))))
  (put 'project '(real) (lambda (x) (project x)))

  'done)

(define (install-project-complex-package)
  (define (project z1)
    (make-real (real-part z1)))
  
  (put 'project '(complex) (lambda (x) (project x)))

  'done)

; exercise 2.86

(define (sine x)
  (apply-generic 'sine x))

(define (cosine x)
  (apply-generic 'cosine x))







