;;; wire

(define (make-wire)
  ;; create a new wire with signal 0 & no action procedures
  (let ([signal 0]
        ;; a list of action procedures to run when signal changes value
        [action-procs '()])
    (define (call-each procedures)
      ;; call each zero-arg procedure in a list of procedures
      (cond [(null? procedures) 'done]
            [else ((car procedures))
                  (call-each (cdr procedures))]))
    (define (set-my-signal! new)
      ;; set the current signal to `new' & call each action procedure.
      ;; do nothing if `new' is the same is the current signal
      (cond [(= signal new) 'done]
            [else (set! signal new)
                  (call-each action-procs)]))
    (define (accept-action-proc proc)
      ;; add `proc' to a list of action procedures
      (set! action-procs
            (cons proc action-procs))
      ;; for some technical reason, we need to add this call.
      ;; e.g. consider a converter whose output needs to be 1 when the
      ;; input is initially 0.
      (proc))
    (define (dispatch m)
      (cond [(eq? m 'get-signal) signal]
            [(eq? m 'set-signal!) set-my-signal!]
            [(eq? m 'add-action!) accept-action-proc]
            [else (error "Unknown operatoin: WIRE" m)]))
    dispatch))

(define (get-signal wire)
  ;; get current signal on wire
  (wire 'get-signal))

(define (set-signal! wire signal)
  ;; change the signal on wire to the new value
  ((wire 'set-signal!) signal))

(define (add-action! wire procdure)
  ;; when the signal on wire changes, call the zero-arg procedure
  ((wire 'add-action!) procdure))

;;; primitive components

(define (logical-not s)
  (cond [(= s 0) 1]
        [else    0]))

(define (logical-and a b)
  (cond [(= a b 1) 1]
        [else      0]))

(define (logical-or a b)
  (cond [(= a b 0) 0]
        [else      1]))

(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay  5)

(define (inverter in out)
  (define (inverter-action-procedure)
    (let ([new (logical-not (get-signal in))])
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! out new)))))
  (add-action! in inverter-action-procedure)
  'ok)

(define (and-gate a b out)
  (define (and-action-procedure)
    (let ([new (logical-and (get-signal a)
                            (get-signal b))])
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! out new)))))
  (add-action! a and-action-procedure)
  (add-action! b and-action-procedure)
  'ok)

(define (or-gate a b out)
  (define (or-action-procedure)
    (let ([new (logical-or (get-signal a)
                           (get-signal b))])
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! out new)))))
  (add-action! a or-action-procedure)
  (add-action! b or-action-procedure)
  'ok)

;;; agenda

;;; aegnda is the cons of the current time & an ordered list of time
;;; segments.  a time segment is a cons of a time & a queue of action
;;; procedures that need to be executed at the time.
(define (make-time-segment time queue) (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (cons 0 '())) ; initially, the current time is 0
(define (current-time agenda) (car agenda))
(define (segments agenda) (cdr agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda)
  (car (segments agenda)))
(define (rest-segments agenda)
  (cdr (segments agenda)))
(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (make-queue)
  (let ([queue (cons '() '())])
    (define (front-ptr) (car queue))
    (define (rear-ptr)  (cdr queue))
    (define (set-front-ptr! item)
      (set-car! queue item))
    (define (set-rear-ptr!  item)
      (set-cdr! queue item))

    (define (empty-queue?) (null? (front-ptr)))

    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue" queue)
          (car (front-ptr))))

    (define (insert-queue! item)
      (let ([pair (cons item '())])
        (if (empty-queue?)
            (set-front-ptr! pair)
            (set-cdr! (rear-ptr) pair))
        (set-rear-ptr! pair)
        queue))

    (define (delete-queue!)
      (if (empty-queue?)
          (error "DELETE! called with an empty queue" queue)
          (begin
            (set-front-ptr! (cdr (front-ptr)))
            queue)))
    (define (dispatch m)
      (cond [(eq? m 'empty-queue?)  empty-queue?]
            [(eq? m 'front-queue)   front-queue]
            [(eq? m 'insert-queue!) insert-queue!]
            [(eq? m 'delete-queue!) delete-queue!]
            [else (error "Unknown operatoin: QUEUE" m)]))
    dispatch))

(define (empty-queue? queue)
  ((queue 'empty-queue?)))
(define (front-queue queue)
  ((queue 'front-queue)))
(define (insert-queue! queue item)
  ((queue 'insert-queue!) item))
(define (delete-queue! queue)
  ((queue 'delete-queue!)))

(define (add-to-agenda! time action agenda)
  ;; agenda has an ordered list of time segments
  (define (belongs-before? segments)
    ;; either reaches the end of segment list, or the current segment
    ;; is later in time
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    ;; create a new time segment, since the ordered segment list in
    ;; agenda doesn't have a segment of current time.  a segment is a
    ;; cons of a time & a queue of action procedures.
    (let ([q (make-queue)])
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (let ([first (car segments)]
          [rest (cdr segments)])
      (if (= (segment-time first) time)
          (insert-queue! (segment-queue first)
                         action)
          (if (belongs-before? rest)
              (set-cdr! segments
                        (cons (make-new-time-segment time action)
                              rest))
              (add-to-segments! rest)))))
  (let ([segments (segments agenda)])
    (if (belongs-before? segments) ; `time' is the earliest in segments
        (set-segments! agenda
                       (cons (make-new-time-segment time action)
                             segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ([q (segment-queue (first-segment agenda))])
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty: FIRST-AGENDA-ITEM")
      (let ([first-seg (first-segment agenda)])
        ;; update agenda time
        (set-current-time! agenda
                           (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

;;;

(define the-agenda (make-agenda))

(define (after-delay delay action)
  ;; do action after some delay
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  ;; run until agenda is empty
  (cond [(empty-agenda? the-agenda) 'done]
        [else ((first-agenda-item the-agenda))
              (remove-first-agenda-item! the-agenda)
              (propagate)]))

(define (probe name wire)
  ;; add probe to wire that displays changes to signal value
  (add-action! wire
               (lambda ()
                 (display name) (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire))
                 (newline))))

;;; example: half adder & full adder

(define (half-adder a b s c)
  (let ([d (make-wire)]
        [e (make-wire)])
    (and-gate a b c)
    (or-gate a b d)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ([s (make-wire)]
        [c1 (make-wire)]
        [c2 (make-wire)])
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

;;; probe: add a monitor to wire, so if the signal on the wire
;;; changes, a message is displayed.
;;; output format: <tag> <current-time> NEW-VALUE <value>
(probe 'sum sum)
; SUM 0  NEW-VALUE = 0

(probe 'carry carry)
; CARRY 0  NEW-VALUE = 0

(half-adder input-1 input-2 sum carry)

(set-signal! input-1 1)
(propagate)

(set-signal! input-2 1)
(propagate)

(set-signal! input-1 0)
(propagate)
