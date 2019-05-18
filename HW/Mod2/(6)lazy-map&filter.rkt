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
;
;
(define naturals
  (letrec ((iter (lambda (i)
                   (lazy-cons i (iter (+ i 1))))))
    (iter 0)))
;
(define (lazy-map proc . xss)
  (define (my-map proc xs)
    (if (null? xs)
        '()
        (cons (proc (car xs)) (my-map proc (cdr xs)))))
  (define (helper yss)
    (lazy-cons (apply proc (my-map lazy-car yss)) (apply lazy-map proc (my-map lazy-cdr yss))))
  (helper xss))
;
(define (lazy-filter pred? xs)
  (if (pred? (lazy-car xs))
      (lazy-cons (lazy-car xs) (lazy-filter pred? (lazy-cdr xs)))
      (lazy-filter pred? (lazy-cdr xs))))