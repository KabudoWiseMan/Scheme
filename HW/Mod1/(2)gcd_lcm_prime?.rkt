(define (my-gcd a b)
  (cond ((or (< a 0) (< b 0)) (my-gcd (abs a) (abs b)))
        ((or (= (remainder a b) 0) (= (remainder b a) 0)) (min a b))
        ((> a b)(my-gcd (remainder a b) b))
        (else (my-gcd a (remainder b a)))))

(define (my-lcm a b)
  ( / (abs (* a b)) (my-gcd a b)))

(define (prime? n)
  (define (helper n i)
    (or (= n i) (and (not (= (remainder n i) 0)) (helper n (+ i 1)))))
  (helper n 2))