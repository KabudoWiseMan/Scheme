;the golden ratio
;
(define fi (/ (+ 1 (sqrt 5)) 2))
;
;program
;
(define (golden f x0 x1 ε)
  (define (helper f x0 x1 ε a b)
    (cond ((< (abs (- x1 x0)) ε) (/ (+ x0 x1) 2))
          ((>= (f a) (f b)) (helper f a x1 ε b (+ a (/ (- x1 a) fi))))
          (else (helper f x0 b ε (- b (/ (- b x0) fi)) a))))
  (helper f x0 x1 ε (- x1 (/ (- x1 x0) fi)) (+ x0 (/ (- x1 x0) fi))))