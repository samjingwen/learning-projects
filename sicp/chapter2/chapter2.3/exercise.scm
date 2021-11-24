#lang sicp

(define (println output)
  (display output)
  (newline))

; exercise 2.53

(list 'a 'b 'c)
(list (list 'george))
(cdr '((x1 x2) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))
(pair? (car '(a short list)))
(memq 'red '((red shoes) (blue socks)))
(memq 'red '(red shoes blue socks))

; exercise 2.54

(println "***2.54")

; (define (equal? a b)
;   (cond ((and (null? a) (null? b)) #t)
;         ((eq? (car a) (car b)) (equal? (cdr a) (cdr b)))
;         (else #f)))

(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))

; exercise 2.55

(car ''abracadabra)
(car (quote (quote abracadabra)))
; quote
(car (quote (list 'a)))


(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-product (make-exponentiation (base exp)
                                                          (make-sum (exponent exp) -1))
                                     (deriv (base exp) var))))
        (else
         (error "unknown expression type: DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (operation expr)
  (if (memq '+ expr)
      '+
      '*))

; (define (sum? x)
;   (and (pair? x) (eq? (cadr x) '+)))

(define (sum? expr)
  (eq? '+ (operation expr)))

; (define (addend s)
;   (car s))

(define (addend expr)
  (define (iter expr result)
    (if (eq? (car expr) '+)
        result
        (iter (cdr expr) (append result (list (car expr))))))
  (let ((result (iter expr '())))
    (if (= (length result) 1)
        (car result)
        result)))

; (define (augend s)
;   (caddr s))

; (define (augend s)
;   (if (> (length s) 3)
;       (cons '+ (cddr s))
;       (caddr s)))

(define (augend expr)
  (let ((result (cdr (memq '+ expr))))
    (if (= (length result) 1)
        (car result)
        result)))

; (define (product? x)
;   (and (pair? x) (eq? (cadr x) '*)))

(define (product? expr)
  (eq? '* (car (memq '* expr))))

; (define (multiplier p)
;   (car p))

(define (multiplier expr)
  (define (iter expr result)
    (if (eq? (car expr) '*)
        result
        (iter (cdr expr) (append result (list (car expr))))))
  (let ((result (iter expr '())))
    (if (= (length result) 1)
        (car result)
        result)))

; (define (multiplicand p)
;   (caddr p))

; (define (multiplicand p)
;   (if (> (length p) 3)
;       (cons '* (cddr p))
;       (caddr p)))

(define (multiplicand expr)
  (let ((result (cdr (memq '* expr))))
    (if (= (length result) 1)
        (car result)
        result)))

(define (=number? exp num) (and (number? exp) (= exp num)))

; (deriv '(+ x 3) 'x)
; 1
; (deriv '(* x y) 'x)
; y
; (deriv '(* (* x y) (+ x 3)) 'x)
; (+ (* x y) (* y (+ x 3)))

; exercise 2.56

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base e) (cadr e))

(define (exponent e) (caddr e))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((and (number? b) (number? e)) (expt b e))
        (else (list '** b e))))

; (deriv '(** (* 2 x) y) 'x)
; (* y (* (** (* 2 x) (+ y -1)) 2))

; exercise 2.57

; (deriv '(* x y 3 (+ x 3 3)) 'x)

; exercise 2.58

(println "***2.58")

(deriv '(x + (3 * (x + (y + 2)))) 'x)
(deriv '(x + 3 * (x + (y + 2))) 'x)

; (x + 3 * (x + y + 2))


; (define (element-of-set? x set)
;   (cond ((null? set) false)
;         ((equal? x (car set)) true)
;         (else (element-of-set? x (cdr set)))))

; (define (adjoin-set x set)
;   (if (element-of-set? x set)
;       set
;       (cons x set)))

; (define (intersection-set set1 set2)
;   (cond ((or (null? set1) (null? set2)) '())
;         ((element-of-set? (car set1) set2)
;          (cons (car set1) (intersection-set (cdr set1) set2)))
;         (else (intersection-set (cdr set1) set2))))

; exercise 2.59

; (define (union-set set1 set2)
;   (cond ((null? set1) set2)
;         ((null? set2) set1)
;         ((element-of-set? (car set1) set2)
;          (union-set (cdr set1) set2))
;         (else (cons (car set1) (union-set (cdr set1) set2)))))

; (union-set '(a b c) '(c d e))

; exercise 2.60

; (define adjoin-set cons)
; (define union-set append)

(define (remove-set x set)
  (cond ((null? set) set)
        ((equal? x (car set)) (cdr set))
        (else (cons (car set) (remove-set x (cdr set))))))

(define (intersection-set-dup set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set-dup (cdr set1)
                                     (remove-set (car set1) set2))))
        (else (intersection-set-dup (cdr set1) set2))))


; (intersection-set '(0 1 1 2 2 3) '(2 3 4))
; (intersection-set-dup '(0 1 1 2 2 3) '(2 3 4))

; 2.59 | 2.60
; element-of-set O(n) | O(n)
; adjoin-set O(n) | O(1)
; union-set O(n^2) | O(n^2)
; intersection-set O(n^2) | O(n^2)

; (define (element-of-set? x set)
;   (cond ((null? set) false)
;         ((= x (car set)) true)
;         ((< x (car set)) false)
;         (else (element-of-set? x (cdr set)))))

; (define (intersection-set set1 set2)
;   (if (or (null? set1) (null? set2))
;       '()
;       (let ((x1 (car set1)) (x2 (car set2)))
;         (cond ((= x1 x2)
;                (cons x1 (intersection-set (cdr set1)
;                                           (cdr set2))))
;               ((< x1 x2)
;                (intersection-set (cdr set1) set2))
;               ((< x2 x1)
;                (intersection-set set1 (cdr set2)))))))

; exercise 2.61

; (define (adjoin-set x set)
;   (cond ((null? set) false)
;         ((= x (car set)) set)
;         ((< x (car set)) (cons x set))
;         (else (cons (car set) (adjoin-set x (cdr set))))))

; (adjoin-set 3 (list 3 4 5))

; exercise 2.62

; (define (union-set set1 set2)
;   (cond ((null? set1) set2)
;         ((= (car set1) (car set2)) (cons (car set1)
;                                          (union-set (cdr set1) (cdr set2))))
;         ((< (car set1) (car set2)) (cons (car set1)
;                                          (union-set (cdr set1) set2)))
;         ((> (car set1) (car set2)) (cons (car set2)
;                                          (union-set (cdr set2) set1)))))

(define (entry tree) (car tree))
; (define (left-branch tree) (cadr tree))
; (define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

; (define (adjoin-set x set)
;   (cond ((null? set) (make-tree x '() '()))
;         ((= x (entry set)) set)
;         ((< x (entry set))
;          (make-tree (entry set)
;                     (adjoin-set x (left-branch set))
;                     (right-branch set)))
;         ((> x (entry set))
;          (make-tree (entry set) (left-branch set)
;                     (adjoin-set x (right-branch set))))))

; inorder traversal
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                     (right-branch tree))))))

; (tree->list-1 '(1 (2 (4 () ()) (5 () ())) (3 () ())))
; (tree->list-1 '(2 (1 () ()) (3 () ())))


(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))

; (tree->list-2 '(1 (2 (4 () ()) (5 () ())) (3 () ())))
; (tree->list-2 '(2 (1 () ()) (3 () ())))


; exercise 2.64

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

; (list->tree (list 1 3 5 7 9 11))
; (list->tree (list 1 3 5))

; (partial-tree '() 0)

; exercise 2.65

(define (merge-list list1 list2)
  (cond ((null? list1) list2)
        ((null? list2) list1)
        ((= (car list1) (car list2)) (cons (car list1)
                                           (merge-list (cdr list1) (cdr list2))))
        ((< (car list1) (car list2)) (cons (car list1)
                                           (merge-list (cdr list1) list2)))
        (else (cons (car list2)
                    (merge-list list1 (cdr list2))))))

(define (union-set set1 set2)
  (let ((list1 (tree->list-2 set1))
        (list2 (tree->list-2 set2)))
    (let ((combined-list (merge-list list1 list2)))
      (list->tree combined-list))))

; (union-set '(4 (2 (1 () ()) (3 () ())) (5 () ())) '(2 (1 () ()) (6 () ())))

(define (intersection-list list1 list2)
  (cond ((or (null? list1) (null? list2)) '())
        ((= (car list1) (car list2)) (cons (car list1)
                                           (intersection-list (cdr list1)
                                                              (cdr list2))))
        ((< (car list1) (car list2)) (intersection-list (cdr list1) list2))
        ((> (car list1) (car list2)) (intersection-list list1 (cdr list2)))))

(define (intersection-set set1 set2)
  (let ((list1 (tree->list-2 set1))
        (list2 (tree->list-2 set2)))
    (let ((combined-list (intersection-list list1 list2)))
      (list->tree combined-list))))

; (intersection-set '(4 (2 (1 () ()) (3 () ())) (5 () ())) '(2 (1 () ()) (6 () ())))

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((= given-key (entry set-of-records)) true)
        ((< given-key (entry set-of-records)) (lookup given-key
                                                      (left-branch set-of-records)))
        (else (lookup given-key (right-branch set-of-records)))))

; (lookup 8 '(4 (2 (1 () ()) (3 () ())) (5 () ())))

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))

; exercise 2.67

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)

; exercise 2.68

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (go result tree)
    (let ((left (left-branch tree))
          (right (right-branch tree)))
      (cond ((and (leaf? left) (eq? (symbol-leaf left) symbol))
             (cons '0 result))
            ((and (leaf? right) (eq? (symbol-leaf right) symbol))
             (cons '1 result))
            ((and (not (leaf? left)) (symbol-in-list? symbol (symbols left)))
             (go (cons '0 result) left))
            ((and (not (leaf? right)) (symbol-in-list? symbol (symbols right)))
             (go (cons '1 result) right))
            (else "error"))))
  (reverse-list (go '() tree)))

; (define (encode-symbol char tree)
;   (cond ((leaf? tree) '())
;         ((memq char (symbols (left-branch tree)))
;          (cons 0 (encode-symbol char (left-branch tree))))
;         ((memq char (symbols (right-branch tree)))
;          (cons 1 (encode-symbol char (right-branch tree))))
;         (else (error "symbol not in tree" char))))


(define (symbol-in-list? symbol list)
  (cond ((null? list) false)
        ((eq? symbol (car list)) true)
        (else (symbol-in-list? symbol (cdr list)))))

(define (reverse-list list)
  (define (go result list)
    (if (null? list)
        result
        (go (cons (car list) result) (cdr list))))
  (go '() list))

(encode-symbol 'D sample-tree)

(encode '(A D A B B C A) sample-tree)

; exercise 2.69

(println "***2.69")

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leafs)
  (cond ((= (length leafs) 1) (car leafs))
        (else (successive-merge (adjoin-set (make-code-tree (car leafs)
                                                            (cadr leafs))
                                            (cddr leafs))))))

(define example-tree
  (generate-huffman-tree '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1))))

(encode '(A D A B B C A) example-tree)
(decode (encode '(A D A B B C A) example-tree) example-tree)

; exercise 2.70

(println "***2.70")

(define rock-song
  (generate-huffman-tree '((A 2) (GET 2) (SHA 3) (WAH 1) (BOOM 1) (JOB 2) (NA 16) (YIP 9))))

rock-song

(length (encode '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM) rock-song))
; 84
; 3 bits per symbol
; 36 * 3 = 108


; exercise 2.71

(define n5
  (generate-huffman-tree '((A 1) (B 2) (C 4) (D 8) (E 16))))

(length (encode '(E) n5))
(length (encode '(A) n5))

(define n10
  (generate-huffman-tree '((A 1) (B 2) (C 4) (D 8) (E 16) (F 32) (G 64) (H 128) (I 256) (J 512))))

(length (encode '(J) n10))
(length (encode '(A) n10))

; exercise 2.72
; https://wizardbook.wordpress.com/2010/12/07/exercise-2-72/







