(load "trace.rkt")
(define (string-split str)
  (define (helper str substring substrings)
    (cond ((null? str) (append substrings (list (list->string substring))))
          ((memq (car str) spaces) (helper (cdr str) substring substrings))
          (else (helper (cdr str) (append substring (list (car str))) substrings))))
  (helper (string->list str) '() '()))
;
(define spaces (string->list " \t\n\r\v\f"))