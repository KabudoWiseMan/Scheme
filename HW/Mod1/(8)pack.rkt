;program from the first lab (first variant third number)
;
(define (replicate x n)
  (if (= n 0)
      '()
      (cons x (replicate x (- n 1)))))
;
;
;
(define (pack given_list)
  (define (helper given_list final_list count element)
    (cond ((null? given_list) (append final_list (list (replicate element count))))
          ((equal? (car given_list) element) (helper (cdr given_list) final_list (+ count 1) element))
          (else (helper given_list (append final_list (list (replicate element count))) 0 (car given_list)))))
    (helper given_list '() 0 (car given_list)))
;
;
;
(define (encode given_list)
  (define (helper given_list final_list count element)
    (cond ((null? given_list) (append final_list (list (list element count))))
          ((equal? (car given_list) element) (helper (cdr given_list) final_list (+ count 1) element))
          (else (helper given_list (append final_list (list (list element count))) 0 (car given_list)))))
    (helper given_list '() 0 (car given_list)))
;
;
;
(define (unpack given_list)
  (define (helper given_list final_list)
    (if (null? given_list)
        final_list
      (helper (cdr given_list) (append final_list (list (replicate (car (car given_list)) (cadr (car given_list))))))))
  (helper given_list '()))
;
;
;
(define (decode given_list)
  (define (helper given_list final_list)
    (if (null? given_list)
        final_list
      (helper (cdr given_list) (append final_list (replicate (car (car given_list)) (cadr (car given_list)))))))
  (helper given_list '()))