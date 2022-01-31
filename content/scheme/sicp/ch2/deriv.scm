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

(define (sum? exp)
  (and [pair? exp]
       [eq? (car exp) '+]))

(define (make-sum a1 a2)
  (cond [(and (number? a1) (number? a2)) (+ a1 a2)]
        [(and (number? a1) (zero? a1)) a2]
        [(and (number? a2) (zero? a2)) a1]
        [else (list '+ a1 a2)]))

(define (a1 exp) (cadr exp))
(define (a2 exp) (caddr exp))

(define (product? exp)
  (and [pair? exp]
       [eq? (car exp) '*]))

(define (make-product m1 m2)
  (cond [(and (number? m1) (number? m2)) (* m1 m2)]
        [(and (number? m1) (= m1 1)) m2]
        [(and (number? m2) (= m2 1)) m1]
        [(or (and (number? m1) (zero? m1))
             (and (number? m2) (zero? m2))) 0]
        [else (list '* m1 m2)]))

(define (m1 exp) (cadr exp))
(define (m2 exp) (caddr exp))

(define (deriv exp var)
  (cond [(constant? exp var) 0]
        [(same-var? exp var) 1]
        [(sum? exp)
         (make-sum (deriv (a1 exp) var)
                   (deriv (a2 exp) var))]
        [(product? exp)
         (make-sum (make-product (m1 exp) (deriv (m2 exp) var))
                   (make-product (m2 exp) (deriv (m1 exp) var)))]))


(define foo '(+ (* a (* x x))           ; ax^2 + bx + c
                (+ (* b x)
                   c)))

(deriv foo 'x)
;;; original:
;; => (+ (+ (* a (+ (* x 1) (* x 1)))
;;          (* (* x x) 0))
;;       (+ (+ (* b 1) (* x 0))
;;          0))
;;; updated make-sum & make-product
;; => (+ (* a (+ x x)) b)
