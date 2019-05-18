(define-syntax lazy-cons
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define (lazy-car ls) (car ls))

(define (lazy-cdr ls) (force (cdr ls)))

(define (lazy-ref ls n)
  (if (= n 0)
      (lazy-car ls)
      (lazy-ref (lazy-cdr ls) (- n 1))))

(define (fib-gen a b)
  (lazy-cons a (fib-gen  b (+ a b))))

(define (lazy-fib n)
  (lazy-ref (fib-gen 0 1) n))