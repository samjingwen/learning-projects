#lang sicp

(define (count-distinct-pairs x)
  (let ((cache nil))

    (define (iter x)
      (if (or (not (pair? x)) (memq x cache))
          0
          (begin (set! cache (cons x cache))
                 (+ 1
                    (iter (car x))
                    (iter (cdr x))))))

    (iter x)))

(define str1 '(foo bar baz))
(count-distinct-pairs str1)

(define x1 '(foo))
(define y1 (cons x1 x1))
(define str2 (list y1))
(count-distinct-pairs str2)

(define x2 '(foo))
(define y2 (cons x2 x2))
(define str3 (cons y2 y2))
(count-distinct-pairs str3)

(define str4 '(foo bar baz))
(set-cdr! (cddr str4) str4)
(count-distinct-pairs str4)


