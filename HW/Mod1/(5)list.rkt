;
;range
;
(define (my-range a b d)
  (if (> b a)
      (append (list a) (my-range (+ a d) b d))
      '()))
;
;flatten
;
(define (my-flatten xs)
  (cond ((null? xs) xs)
        ((list? (car xs)) (my-flatten (append (car xs) (cdr xs))))
        (else (append (list (car xs)) (my-flatten (cdr xs))))))
;
;my-element?
;
(define (my-element? x xs)
  (and (not (null? xs)) (or (equal? x (car xs)) (my-element? x (cdr xs)))))
;
;my-filter
;
(define (my-filter pred? xs)
  (cond ((null? xs) '())
        ((pred? (car xs)) (cons (car xs) (my-filter pred? (cdr xs))))
        (else (my-filter pred? (cdr xs)))))
;
;my-fold-left
;
(define (my-fold-left op xs)
  (if (= (length xs) 1)
      (car xs)
      (my-fold-left op (append (list (op (car xs) (cadr xs))) (cddr xs)))))
;
;my-fold-right
;
(define (my-fold-right op xs)
  (if (= (length xs) 1)
      (car xs)
      (my-fold-right op (append  (cddr (reverse xs)) (list (op (cadr (reverse xs)) (car (reverse xs))))))))