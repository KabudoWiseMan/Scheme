(define (make-get type names c) 
  (define (make-get-helper type name f) 
    (quasiquote (define 
                  (,(string->symbol (list->string (append type '(#\-) (string->list (symbol->string name))))) a) 
                  (vector-ref a ,f)))) 
  (if (not (null? names)) 
      (begin 
        (eval (make-get-helper type (car names) c) 
              (interaction-environment)) 
        (make-get type (cdr names) (+ c 1))))) 

(define (make-set type names c) 
  (define (make-set-helper type name f) 
    (quasiquote (define 
                  (,(string->symbol (list->string (append (string->list "set-") type '(#\-) (string->list (symbol->string name)) '(#\!)))) a val) 
                  (vector-set! a ,f val)))) 
  (if (not (null? names)) 
      (begin 
        (eval (make-set-helper type (car names) c) 
              (interaction-environment)) 
        (make-set type (cdr names) (+ c 1))))) 

(define-syntax define-struct 
  (syntax-rules () 
    ((_ struct-type . field-names) 
     (let ((names (car (quasiquote field-names))) 
           (type (string->list (symbol->string (quasiquote struct-type))))) 
       (begin 
         (eval (quasiquote 
                (define (,(string->symbol (list->string (append type '(#\?)))) a) 
                  (equal? (vector-ref a 0) ,(list->string type)))) 
               (interaction-environment)) 
         (eval (quasiquote 
                (define (,(string->symbol (list->string (append (string->list "make-") type))) . a) 
                  (list->vector (cons ,(list->string type) a)))) 
               (interaction-environment)) 
         (make-get type names 1) 
         (make-set type names 1))))))