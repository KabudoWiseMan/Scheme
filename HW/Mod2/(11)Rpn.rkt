(define priors '((< 0) (+ 1) (- 1) (* 2) (/ 2)))
;
(define (check-priors x1 x2)
  (and (<= (cadr (assq x1 priors)) (cadr (assq x2 priors)))))
(define (pusher x xss)
  ;
  (cond ((null? xss) (list x))
        ((check-priors x (car xss)) (pusher x (cdr xss)))
        (else (cons x xss))))
;
(define (pushed x xss t)
  (cond ((null? xss) t)
        ((check-priors x (car xss)) (pushed x (cdr xss) (append t (list (car xss)) )))
        (else t)))
;
(define (bracket-pushed xss t)
  (if (equal? '< (car xss))
      t
      (bracket-pushed (cdr xss) (append t (list (car xss))))))
;
(define (bracket-pusher xss)
  (if (equal? '< (car xss))
      (cdr xss)
      (bracket-pusher (cdr xss))))
;
(define (polsa xss result stack)
  (cond ((null? xss) (append result stack))
        ((or (equal? (car xss) '+) (equal? (car xss) '*) (equal? (car xss) '-) (equal? (car xss) '/))
         (polsa (cdr xss) (append result (pushed (car xss) stack '())) (pusher (car xss) stack)))
        ((equal? (car xss) '<) (polsa (cdr xss) result
                                      (append (list '<) stack)))
        ((equal? (car xss) '>) (polsa (cdr xss) (append result (bracket-pushed stack '())) (bracket-pusher stack)))
        (else (polsa (cdr xss) (append result (list (car xss))) stack))))
;
(define (rpn xss)
  (else (polsa xss '() '())))