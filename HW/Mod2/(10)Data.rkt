(define (symbol-append sym1 sym2)
  (string->symbol (string-append (symbol->string sym1) (symbol->string sym2))))
;
(define-syntax make-pred
  (syntax-rules ()
    ((_ name)
     (eval
      `(define (,(symbol-append name '?) data)
         (and (vector? data) (not (= (vector-length data) 0)) (equal? name (vector-ref data 0))))        
      (interaction-environment)))))
;
(define-syntax make-figures
  (syntax-rules ()
    ((_ name fields)
     (map
      (lambda (field)
        (eval
         `(define (,(car field) ,@(cdr field))
            (list->vector (list name (quote ,(car field)) ,@(cdr field))))
         (interaction-environment)))
      (car fields)))))
;
(define (match-helper pattern-vec templates-xs)
       (if (equal? (vector-ref pattern-vec 1) (caaar templates-xs))
           (eval
            `(let ,(map list (cdaar templates-xs) (cddr (vector->list pattern-vec)))
               ,(cadar templates-xs))
            (interaction-environment))
           (match-helper pattern-vec (cdr templates-xs))))
;
(define-syntax match
  (syntax-rules ()
    ((_ pattern . templates)
     (match-helper pattern 'templates))))
;
(define-syntax define-data
  (syntax-rules ()
    ((_ name . fields)
     (begin
       (make-pred 'name)
       (make-figures 'name (quote fields))
       ))))