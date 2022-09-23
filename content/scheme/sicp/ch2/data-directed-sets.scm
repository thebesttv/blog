(define table (make-hash-table))

(define (put op type item)
  (hash-set! table (list op type) item))
(define (get op type)
  (hash-ref table (list op type)))

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args))) ; type tag of each argument
    (let ((proc (get op type-tags)))     ; get procedure in table
      (if proc
          (apply proc (map contents args))
          (error "No method for these types: APPLY-GENERIC"
                 (list op type-tags))))))



(define list-set
  '(list (the  (address "Adam Knigdom")
               (salary 1024))
         (best (address "Bata Road")
               (salary 2048))
         (tv   (address "Cindy Street")
               (salary 4096))))

(type-tag list-set)
(contents list-set)

(define (key record) (car record))
(define (entries record) (cdr record))

(key (car (contents list-set)))
(entries (car (contents list-set)))
