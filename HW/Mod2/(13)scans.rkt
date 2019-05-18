(define (make-source sequence . end-symbol)
  (let*
      ((buffer
        (cond
          ((vector? sequence) sequence)
          ((list? sequence) (list->vector sequence))
          ((string? sequence) (list->vector (string->list sequence)))))
       (size (vector-length buffer))
       (pos 0)
       (end (and (not (null? end-symbol)) (car end-symbol))))
    (vector buffer size pos end)))
;
(define (peek sequence)
  (let ((seq (vector-ref sequence 0))
        (len (vector-ref sequence 1))
        (pos (vector-ref sequence 2))
        (end (vector-ref sequence 3)))
    (if (< pos len)
        (vector-ref seq pos)
        end)))
;
(define-syntax next!
  (syntax-rules ()
    ((_ sequence) (let ((seq (vector-ref sequence 0))
                        (len (vector-ref sequence 1))
                        (pos (vector-ref sequence 2))
                        (end (vector-ref sequence 3)))
                    (if (< pos len)
                        (begin (vector-set! sequence 2 (+ pos 1))                              
                               (vector-ref seq pos))
                        end)))))
;
;programs
;
;<Final> ::= <Sign> <Number> | <Number> .
;<Number> ::= <Number> <Digit> | <Digit> .
;<Sign> ::= "-" | "+" .
;<Digit> ::= "0" | "1" | ... | "9" .
;
(define (check-integer str)
  (define src (make-source str #\null))
  (define (end? char) (and (eq? char (vector-ref src 3)) (= (vector-ref src 2) (vector-ref src 1))))
  (define (sign? char) (and (or (eq? char #\-) (eq? char #\+)) (= (vector-ref src 2) 1)))
  (define (digit? char) (and (>= (char->integer char) 48) (<= (char->integer char) 57)))
  (call-with-current-continuation
   (lambda (fail)
     (begin
       (define (continue-scan)
         (let ((ch (next! src)))
           (cond
             ((null? (string->list str)) #f)
             ((end? ch) #t)
             ((and (= (vector-ref src 1) 1) (sign? ch)) #f)             
             ((sign? ch) (continue-scan))
             ((digit? ch) (continue-scan))
             (else (fail #f)))))
       (continue-scan)))))
;
(define (scan-integer str)
  (define src (make-source str #\null))
  (define (end? char) (and (eq? char (vector-ref src 3)) (= (vector-ref src 2) (vector-ref src 1))))
  (define (sign? char) (and (or (eq? char #\-) (eq? char #\+)) (= (vector-ref src 2) 1)))
  (define (digit? char) (and (>= (char->integer char) 48) (<= (char->integer char) 57)))
  (call-with-current-continuation
   (lambda (fail)
     (begin
       (define (continue-scan)
         (let ((ch (next! src)))
           (cond
             ((null? (string->list str)) #f)
             ((end? ch) 0)
             ((and (= (vector-ref src 1) 1) (sign? ch)) #f) 
             ((sign? ch) (or (and (eq? ch #\-) (- (continue-scan))) (continue-scan)))
             ((digit? ch) (+ (* (- (char->integer ch) 48) (expt 10 (- (vector-ref src 1) (vector-ref src 2)))) (continue-scan)))
             (else (fail #f)))))
       (continue-scan)))))
;
(define spaces (string->list " \t\n\r\v\f"))
;
(define (scan-many-integers str)
  (define src (make-source str #\null))
  (define (helper1 str l) 
    (cond
      ((null? str) l)
      ((memq (car str) spaces) (helper1 (cdr str) l ))
      (else (helper2 str l '()))))
  (define (helper2 str l x) 
    (cond
      ((null? str)
       (and
        (check-integer (list->string x))
        (append l (list (scan-integer (list->string x))))))
      ((memq (car str) spaces) 
       (and
        (check-integer (list->string x))
        (helper1 (cdr str) (append l (list (scan-integer (list->string x)))))))
      (else (helper2 (cdr str) l (append x (list (car str)))))))
  (helper1 (string->list str) '()))
;
;tests
;
(load "unit-testing.rkt")
(define the-tests
  (list (test (check-integer "+123") '#t)
        (test (check-integer "-123") '#t)
        (test (check-integer "1234") '#t)
        (test (check-integer "1") '#t)
        (test (check-integer "1/2") '#f)
        (test (check-integer "+") '#f)
        ;
        (test (scan-integer "+123") '123)
        (test (scan-integer "-123") '-123)
        (test (scan-integer "1234") '1234)
        (test (scan-integer "1") '1)
        (test (scan-integer "1/2") '#f)
        (test (scan-integer "1+1") '#f)
        ;
        (test (scan-many-integers "\t1\t\t-2\n3")  '(1 -2 3))
        (test (scan-many-integers "  123 +123 -123  ") '(123 123 -123))
        (test (scan-many-integers "1+ 1") '#f)))
;
(run-tests the-tests)