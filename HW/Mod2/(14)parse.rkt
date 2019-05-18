; tokenizer
(define (tokenize str)
  (define error 'kek)
  ; Вспомогательная функция дли анализа строки
  (define (helper expr)
    ; make-number
    (define (make-number xs)
      (cond
        ((string->number (list->string xs)))
        (else (string->symbol (list->string xs)))))
    ; lit?
    (define (lit? expr xs)
      (cond
        ((and
          (not (null? expr)) 
          (char-alphabetic? (car expr))) 
         (lit? (cdr expr) (append xs (list (car expr)))))
        (else (list expr xs))))
    ; num?
    (define (num? expr xs)
      (cond
        ((not (null? expr))
         (let ((num (car expr)))
           (cond
             ((char-numeric? num) (num? (cdr expr) (append xs (list num)))) 
             ((or (equal? num #\e) (equal? num #\E))
              (cond
                ((and
                  (not (null? (cdr expr)))
                  (or (equal? (cadr expr) #\+) (equal? (cadr expr) #\-)))
                 (num? (cddr expr) (append xs (list num) (list (cadr expr)))))
                (else (num? (cdr expr) (append xs (list num))))))
             ((equal? num #\.) (num? (cdr expr) (append xs (list num))))
             (else (list expr xs)))))
        (else (list expr xs))))
    (cond
      ((null? expr) '())
      (else
       (let ((lit (car expr)))
         (cond
           ((equal? lit #\() (cons "(" (helper (cdr expr))))
           ((equal? lit #\)) (cons ")" (helper (cdr expr))))
           ((equal? lit #\+) (cons '+ (helper (cdr expr))))
           ((equal? lit #\-) (cons '- (helper (cdr expr))))
           ((equal? lit #\*) (cons '* (helper (cdr expr))))
           ((equal? lit #\/) (cons '/ (helper (cdr expr))))
           ((equal? lit #\^) (cons '^ (helper (cdr expr))))
           ((char-alphabetic? lit)
            (let ((xs (lit? expr '()))) (cons (string->symbol (list->string (cadr xs))) (helper (car xs)))))
           ((char-numeric? lit)
            (let ((xs (num? expr '()))) (cons (make-number (cadr xs)) (helper (car xs)))))
           ((char-whitespace? lit) (helper (cdr expr)))
           (else (error #f)))))))
  (call-with-current-continuation
   (lambda (exit)
     (set! error exit)
     (helper (string->list str)))))
;
; parser
(define (parse expr)
  (define error 'kek)
  ; peek
  (define (peek)
    (cond
      ((null? expr) #f)
      (else (car expr))))
  ; next
  (define (next)
    (let ((key (peek)))
      (cond (key (set! expr (cdr expr))))
      key))
  ; Вспомогательная функция 0 - для переменных/чисел/скобок
  (define (parse-num-brac)
    (let ((a (next)))
      (cond
        ((number? a) a)
        ((equal? a "(")
         (let ((res (parse-sum-sub)))
           (cond
             ((equal? (next) ")") 
              res)
             (else (error #f)))))
        ((equal? a '-) (list '- (parse-num-brac)))
        ((and
          (symbol? a)
          (not (member a '(+ * / =))))
         a)
        (else (error #f)))))
  ; Вспомогательная функция 1 - для "+" и "-"
  (define (parse-sum-sub)
    (define (help xs)
      (let ((a (peek)))
        (cond
          ((or (equal? a '+) (equal? a '-))
           (help (list xs (next) (parse-prod-div))))
          (else xs))))
    (help (parse-prod-div)))
  ; Вспомогательная функция 2 - для "*" и "/"
  (define (parse-prod-div)
    (define (help xs)
      (let ((a (peek)))
        (cond
          ((or (equal? a '*) (equal? a '/))
           (help (list xs (next) (parse-pow))))
          (else xs))))
    (help (parse-pow)))
  ;
  ; Вспомогательная функция 3 - для "^"
  (define (parse-pow)
    (let ((xs (parse-num-brac)))
      (cond
        ((equal? (peek) '^)
         (list xs (next) (parse-pow)))
        (else xs))))
  
  (call-with-current-continuation
   (lambda (exit) 
     (set! error exit)
     (let ((res (parse-sum-sub)))
       (and (not (next)) res)))))
;
; tree->scheme
(define (tree->scheme expr)
  (cond
    ((and (list? expr) (equal? (length expr) 3))
     (cond
       ((equal? (cadr expr) '^)
        (list 'expt (tree->scheme (car expr)) (tree->scheme (caddr expr))))
       (else (list (cadr expr) (tree->scheme (car expr)) (tree->scheme (caddr expr))))))
    (else expr)))