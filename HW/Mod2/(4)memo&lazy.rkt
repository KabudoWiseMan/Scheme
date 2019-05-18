(define memoized-factorial
  (let ((memo '()))
    (lambda (n)
      (let ((memoized (assq n memo)))
        (if memoized
            (cadr memoized)
            (let ((new-value
                   (if (= n 0)
                       1
                       (* n (memoized-factorial (- n 1))))))
              (set! memo (cons (list n new-value) memo))
              new-value))))))
;
;
;
(define-syntax lazy-cons
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))
;
(define (lazy-car xs) (car xs))
;
(define (lazy-cdr xs) (force (cdr xs)))
;
(define (lazy-head xs k)
  (if (= k 0)
      '()
      (cons (lazy-car xs) (lazy-head (lazy-cdr xs) (- k 1)))))
;
(define (lazy-ref xs n)
  (if (= n 0)
      (lazy-car xs)
      (lazy-ref (lazy-cdr xs) (- n 1))))
;
(define (naturals start)
  (lazy-cons start (naturals (+ start 1))))
;
(define (lazy-factorial n)
  (define (fact-gen n fact)
    (lazy-cons fact (fact-gen (+ n 1) (memoized-factorial n))))
  (lazy-ref (fact-gen 1 1) n))