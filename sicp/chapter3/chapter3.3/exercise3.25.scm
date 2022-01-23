#lang sicp

(define (make-table same-key?)
  (let ((local-table (list '*table*)))

    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))

    (define (lookup . keys)
      (define (iter keys parent-table)
        (cond ((null? keys) (if (pair? (cdr parent-table))
                                false
                                (cdr parent-table)))
              (else (let ((subtable (assoc (car keys)
                                           (cdr parent-table))))
                      (if subtable
                          (iter (cdr keys) subtable)
                          false)))))

      (iter keys local-table))

    (define (insert! value . keys)
      (define (iter keys parent-table)
        (cond ((null? keys) (set-cdr! parent-table value) 'ok)
              (else (let ((subtable (assoc (car keys)
                                           (cdr parent-table)))
                          (new-table (cons (car keys) '())))
                      (cond ((and subtable (pair? (cdr subtable)))
                             (iter (cdr keys) subtable))
                            ; old value will also be replaced with new empty table
                            (else (set-cdr! parent-table
                                            (cons new-table (cdr parent-table)))
                                  (iter (cdr keys) new-table)))))))
      (iter keys local-table))

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))

    dispatch))

(define operation-table (make-table eq?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put 1234 'letters)
(get 'letters)

(put 123 'letters 'small)
(get 'letters 'small)

(put 65 'letters 'small 'a)
(get 'letters 'small 'a)

(put 1 'math '+)
(put 2 'math '+)
(get 'math '+)

(put 65 'letters 'small 'ascii 'a)
(get 'letters 'small 'ascii 'a)

