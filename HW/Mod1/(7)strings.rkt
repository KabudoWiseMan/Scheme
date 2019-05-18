(define (string-trim-left given_string)
  (define (helper given_string_to_list)
    (if (or (eq? (car given_string_to_list) '#\tab)
            (eq? (car given_string_to_list) '#\newline)
            (eq? (car given_string_to_list) '#\space))
        (helper (cdr given_string_to_list))
        (list->string given_string_to_list)))
  (helper (string->list given_string)))
;
;
;
(define (string-trim-right given_string)
  (define (helper given_string_to_list)
    (if (or (eq? (car given_string_to_list) '#\tab)
            (eq? (car given_string_to_list) '#\newline)
            (eq? (car given_string_to_list) '#\space))
        (helper (cdr given_string_to_list))
        (list->string (reverse given_string_to_list))))
  (helper (reverse (string->list given_string))))
;
;
;
(define (string-trim given_string)
  (string-trim-right (string-trim-left given_string)))
;
;
;
(define (string-prefix? a b)
  (define (helper list_a list_b)
    (cond ((null? list_a) #t)
          ((null? list_b) #f)
          ((> (length list_a) (length list_b)) #f)
          (else (and (eq? (car list_a) (car list_b)) (helper (cdr list_a) (cdr list_b))))))
  (helper (string->list a) (string->list b)))
;
;
;
(define (string-suffix? a b)
  (define (helper list_a list_b)
    (cond ((null? list_a) #t)
          ((null? list_b) #f)
          ((> (length list_a) (length list_b)) #f)
          (else (and (eq? (car list_a) (car list_b)) (helper (cdr list_a) (cdr list_b))))))
  (helper (reverse (string->list a)) (reverse (string->list b))))
;
;
;
(define (string-infix? a b)
  (define (helper a b length_a length_b)
    (cond ((= length_a length_b) (string-prefix? a b))
          ((> length_a length_b) #f)
          (else (or (string-prefix? a b) (helper a (list->string (cdr (string->list b))) length_a (- length_b 1))))))
  (helper a b (length (string->list a)) (length (string->list b))))
;
;
;
(define (string-split str sep)
  (define (helper str sep cdr_sep substring substrings)
    (cond ((null? str) (append substrings (list (list->string substring))))
          ((null? cdr_sep) (helper str sep sep '() (append substrings (list (list->string substring)))))
          ((eq? (car str) (car cdr_sep)) (helper (cdr str) sep (cdr cdr_sep) substring substrings))
          (else (helper (cdr str) sep sep (append substring (list (car str))) substrings))))
  (helper (string->list str) (string->list sep) (string->list sep) '() '()))