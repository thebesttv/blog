(define (make-table pred)
  (let ([table (list '*table*)]
        [none (list '())])              ; assuming '() is never used as key
    (define (assoc key records)
      (cond [(null? records) #f]
            [(pred key (caar records)) (car records)]
            [else (assoc key (cdr records))]))
    (define (lookup keys)
      (let iter ([keys (append keys none)] [table table])
        (if (null? keys)
            (cdr table)
            (let ([record (assoc (car keys) (cdr table))])
              (if record
                  (iter (cdr keys) record)
                  #f)))))
    (define (insert! keys value)
      (let iter ([keys (append keys none)] [table table])
        (if (null? keys)
            (set-cdr! table value)
            (let ([record (assoc (car keys) (cdr table))])
              (if record
                  (iter (cdr keys) record)
                  (set-cdr! table
                            (cons (iter (cdr keys) (list (car keys)))
                                  (cdr table))))))
        table))
    (define (dispatch m)
      (cond [(eq? m 'lookup-proc) lookup]
            [(eq? m 'insert-proc!) insert!]
            [(eq? m 'peek) table]
            [else (error "Unknown operatoin: TABLE" m)]))
    dispatch))

(define table (make-table equal?))
(define (get keys)
  (display ((table 'lookup-proc) keys))
  (newline))
(define (put keys value)
  (display ((table 'insert-proc!) keys value))
  (newline))

(put '(a)     1)  ; (*table* (a (() . 1)))
(put '(a b)   2)  ; (*table* (a (b (() . 2)) (() . 1)))
(put '(a c)   3)  ; (*table* (a (c (() . 3)) (b (() . 2)) (() . 1)))
(put '(a b c) 4)  ; (*table* (a (c (() . 3)) (b (c (() . 4)) (() . 2)) (() . 1)))
(put '(a c c) 5)  ; (*table* (a (c (c (() . 5)) (() . 3)) (b (c (() . 4)) (() . 2)) (() . 1)))
(get '(a))        ; 1
(get '(a b))      ; 2
(get '(a c))      ; 3
(get '(a b c))    ; 4
(get '(a c c))    ; 5

(put '(a)     6)  ; (*table* (a (c (c (() . 5)) (() . 3)) (b (c (() . 4)) (() . 2)) (() . 6)))
(put '(a b)   7)  ; (*table* (a (c (c (() . 5)) (() . 3)) (b (c (() . 4)) (() . 7)) (() . 6)))
(put '(a c)   8)  ; (*table* (a (c (c (() . 5)) (() . 8)) (b (c (() . 4)) (() . 7)) (() . 6)))
(put '(a b c) 9)  ; (*table* (a (c (c (() . 5)) (() . 8)) (b (c (() . 9)) (() . 7)) (() . 6)))
(put '(a c c) 10) ; (*table* (a (c (c (() . 10)) (() . 8)) (b (c (() . 9)) (() . 7)) (() . 6)))
(get '(a))        ; 6
(get '(a b))      ; 7
(get '(a c))      ; 8
(get '(a b c))    ; 9
(get '(a c c))    ; 10
