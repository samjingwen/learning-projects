#lang sicp

(define make-entry cons)
(define (make-tree entry left right)
  (list entry left right))

(define (select-entry tree) (car tree))
(define (select-key tree) (caar tree))
(define (select-value tree) (cdar tree))
(define (set-value! tree value) (set-cdr! (car tree) value))

(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (assoc key records)
  (cond ((null? records) false)
        ((= key (select-key records)) records)
        ((< key (select-key records)) (assoc key
                                             (left-branch records)))
        ((> key (select-key records)) (assoc key
                                             (right-branch records)))))

(define (adjoin-tree x tree)
  (cond ((null? tree) x)
        ((null? x) tree)
        ((= (select-key x) (select-key tree)) tree)
        ((< (select-key x) (select-key tree))
         (make-tree (select-entry tree)
                    (adjoin-tree x (left-branch tree))
                    (right-branch tree)))
        ((> (select-key x) (select-key tree))
         (make-tree (select-entry tree)
                    (left-branch tree)
                    (adjoin-tree x (right-branch tree))))))

(define (make-table)
  (let ((local-table (make-tree (make-entry 0 '()) '() '())))

    (define (lookup . keys)
      (define (iter keys parent-table)
        (cond ((null? keys) (if (pair? (select-value parent-table))
                                false
                                (select-value parent-table)))
              (else (let ((subtable (assoc (car keys)
                                           (select-value parent-table))))
                      (if subtable
                          (iter (cdr keys) subtable)
                          false)))))
      (iter keys local-table))

    (define (insert! value . keys)
      (define (iter keys parent-table)
        (cond ((null? keys) (set-value! parent-table value) 'ok)
              (else (let ((subtable (assoc (car keys) (select-value parent-table)))
                          (new-table (make-tree (make-entry (car keys) '())
                                                '() '())))
                      (cond ((and subtable (pair? (select-value subtable)))
                             (iter (cdr keys) subtable))
                            (else (set-value! parent-table
                                              (adjoin-tree (select-value parent-table)
                                                           new-table))
                                  (iter (cdr keys) new-table)))))))
      (iter keys local-table))

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))

    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put 'a 1)
(put 'b 1)
(get 1)
(put 'b 2)
(get 1)
(get 2)

(put 'c 1 2)
(put 'd 2 3)
(get 1 2)
(get 2 3)

(put 'c 1 2 3)
(get 1 2 3)

(put 'd 3 4)
(get 3 4)

