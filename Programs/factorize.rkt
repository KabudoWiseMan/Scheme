(define (a2-b2? expr)
  (and (list? expr)
       (= (length expr) 3)
       (eq? (car expr) '-)
       (list? (cadr expr))
       (eq? 'expt (car (cadr expr)))
       (eq? 'expt (car (caddr expr)))
       (= 2 (caddr (cadr expr)))
       (= 2 (caddr (caddr expr)))))

(define (a3-b3? expr)
  (and (list? expr)
       (= (length expr) 3)
       (eq? (car expr) '-)
       (list? (cadr expr))
       (eq? 'expt (car (cadr expr)))
       (eq? 'expt (car (caddr expr)))
       (= 3 (caddr (cadr expr)))
       (= 3 (caddr (caddr expr)))))

(define (a3+b3? expr)
  (and (list? expr)
       (= (length expr) 3)
       (eq? (car expr) '+)
       (list? (cadr expr))
       (eq? 'expt (car (cadr expr)))
       (eq? 'expt (car (caddr expr)))
       (= 3 (caddr (cadr expr)))
       (= 3 (caddr (caddr expr)))))

(define (factorize expression)
  (cond ((a2-b2? expression) (append '(*) (list (list '- (cadr (cadr expression)) (cadr (caddr expression)))) (list (list '+ (cadr (cadr expression)) (cadr (caddr expression))))))
        ((a3-b3? expression) (append '(*) (list (list '- (cadr (cadr expression)) (cadr (caddr expression)))) (list (list '+ (list 'expt (cadr (cadr expression)) '2) (list '* (cadr (cadr expression)) (cadr (caddr expression))) (list 'expt (cadr (caddr expression)) '2)))))
        ((a3+b3? expression) (append '(*) (list (list '+ (cadr (cadr expression)) (cadr (caddr expression)))) (list (list '+ (list '- (list 'expt (cadr (cadr expression)) '2) (list '* (cadr (cadr expression)) (cadr (caddr expression)))) (list 'expt (cadr (caddr expression)) '2)))))
        (else #f)))