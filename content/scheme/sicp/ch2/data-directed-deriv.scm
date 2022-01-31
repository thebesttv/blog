(define (atom? exp)
  (not (pair? exp)))

(define (constant? exp var)
  ;; whether the expression (exp) is a constant
  ;; with respect to the variable (var)
  (and [atom? exp]
       [not (eq? exp var)]))

(define (same-var? exp var)
  (and [atom? exp]
       [eq? exp var]))

(define put '())
(define get '())
(let ([table (make-hash-table)])        ; hide `table' from other functions
  (set! put
        (lambda (op type item)
          (hash-set! table (list op type) item)))
  (set! get
        (lambda (op type)
          (hash-ref table (list op type)))))

(define (install-symbolic-deriv-package)
  ;; same as in deriv.scm
  (define (make-sum a1 a2)
    (cond [(and (number? a1) (number? a2)) (+ a1 a2)]
          [(and (number? a1) (zero? a1)) a2]
          [(and (number? a2) (zero? a2)) a1]
          [else (list '+ a1 a2)]))

  (define (a1 exp) (cadr exp))
  (define (a2 exp) (caddr exp))

  (define (make-product m1 m2)
    (cond [(and (number? m1) (number? m2)) (* m1 m2)]
          [(and (number? m1) (= m1 1)) m2]
          [(and (number? m2) (= m2 1)) m1]
          [(or (and (number? m1) (zero? m1))
               (and (number? m2) (zero? m2))) 0]
          [else (list '* m1 m2)]))

  (define (m1 exp) (cadr exp))
  (define (m2 exp) (caddr exp))

  ;; deriv-sum & deriv-product
  (define (deriv-sum exp var)
    (make-sum (deriv (a1 exp) var)
              (deriv (a2 exp) var)))

  (define (deriv-product exp var)
    (make-sum (make-product (m1 exp) (deriv (m2 exp) var))
              (make-product (m2 exp) (deriv (m1 exp) var))))

  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)

  'done)

(install-symbolic-deriv-package)

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (deriv exp var)
  (cond [(constant? exp var) 0]
        [(same-var? exp var) 1]
        [else ((get 'deriv (operator exp))
               exp var)]))

(define foo '(+ (* a (* x x))           ; ax^2 + bx + c
                (+ (* b x)
                   c)))

(deriv foo 'x)                          ; (+ (* a (+ x x)) b)
