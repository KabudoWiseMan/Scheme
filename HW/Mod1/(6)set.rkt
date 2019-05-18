;list->set
;
(define (list->set xs)
  (cond ((null? xs) '())
        ((member (car xs) (cdr xs)) (list->set (cdr xs)))
        (else (cons (car xs) (list->set (cdr xs))))))
;
;set?
;
(define (set? xs)
  (define (helper xs counter)
    (cond ((and (null? xs) (= counter 0)) #t)
          ((and (null? xs) (> counter 0)) #f)
          (else (not (or (list? (member (car xs) (cdr xs))) (helper (cdr xs) (+ counter 1)))))))
  (helper xs 0))
;
;union
;
(define (union xs ys)
  (list->set (append xs ys)))
;
;intersection
;
(define (intersection xs ys)
  (define (helper xz)
    (cond ((null? xz) '())
          ((member (car xz) (cdr xz)) (cons (car xz) (helper (cdr xz))))
          (else (helper (cdr xz)))))
  (helper (append xs ys)))
;
;difference
;
(define (difference xs ys)
  (cond ((null? xs) '())
        ((member (car xs) ys) (difference (cdr xs) (cdr ys)))
        (else (cons (car xs) (difference (cdr xs) ys)))))
;
;symmetric-difference
;
(define (symmetric-difference xs ys)
  (define (helper xs ys)
    (cond ((null? xs) '())
          ((member (car xs) ys) (difference (cdr xs) (cdr ys)))
          (else (cons (car xs) (difference (cdr xs) ys)))))
  (helper (union xs ys) (intersection xs ys)))
;
;set-eq?
;
(define (set-eq? xs ys)
  (and (= (length (intersection xs ys)) (length xs)) (= (length (intersection xs ys)) (length ys))))