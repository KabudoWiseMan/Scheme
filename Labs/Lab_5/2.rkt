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
;<Program>  ::= <Articles> <Body> .
;<Articles> ::= <Article> <Articles> | .
;<Article>  ::= define word <Body> end .
;<Body>     ::= if <Body> endif <Body> | integer <Body> | word <Body> | .
;
(define (parse expr)
  (define src (make-source expr #\nul))
  (call-with-current-continuation
   (lambda (fail)
     (begin
       (define (article)
         (next! src)
         (cond
           ((equal? (peek src) 'end)
            (begin (next! src) '()))
           ((equal? (peek src) 'if)
            (cons (list (peek src) (body-end)) (article)))
           ((= (vector-ref src 1)
               (vector-ref src 2)) (fail #f))
           (else (cons (peek src) (article)))))
       (define (body-end)
         (next! src)
         (if (= (- (vector-ref src 1) 1)
                (vector-ref src 2))
             (begin (next! src) '())
             (if (equal? (peek src) 'endif)
                 '()
                 (cons (peek src) (body-end)))))    
       (define (articles)
         (next! src)
         (list (peek src) (article)))
       (define (body)
         (next! src)
         (list (peek src) (body-end)))
       (define (program)
         (if (not (equal? (peek src) #\nul))
             (let* ((ch (peek src))
                    (pos (vector-ref src 2)))
               (cond
                 ((equal? ch 'define) (cons (articles) (program)))
                 ((= pos 0) (list '() (cons (peek src) (begin (next! src) (program)))))
                 ((equal? ch 'if) (cons (list (peek src) (body-end)) (program)))
                 (else (cons (peek src) (begin (next! src) (program))))))
             '()))
       (program)))))
;
;tests
;
(load "unit-testing.rkt")
;
(define the-tests
  (list (test (parse #(1 2 +)) '(() (1 2 +)))
        (test (parse #(x dup 0 swap if drop -1 endif)) '(() (x dup 0 swap (if (drop -1)))))
        (test (parse #( define -- 1 - end 
                         define =0? dup 0 = end 
                         define =1? dup 1 = end 
                         define factorial 
                         =0? if drop 1 exit endif 
                         =1? if drop 1 exit endif 
                         dup -- 
                         factorial 
                         * 
                         end 
                         0 factorial 
                         1 factorial 
                         2 factorial 
                         3 factorial 
                         4 factorial ))
              '((-- (1 -))
                 (=0? (dup 0 =))
                 (=1? (dup 1 =))
                 (factorial
                  (=0? (if (drop 1 exit)) =1? (if (drop 1 exit)) dup -- factorial *))
                0 factorial 1 factorial 2 factorial 3 factorial 4 factorial))
        (test (parse #(   define =0? dup 0 = end 
                           define gcd 
                           =0? if drop exit endif 
                           swap over mod 
                           gcd 
                           end 
                           90 99 gcd 
                           234 8100 gcd    ))
              '((=0? (dup 0 =)) (gcd (=0? (if (drop exit)) swap over mod gcd)) 90 99 gcd 234 8100 gcd))
        (test (parse #(   define =0? dup 0 = end 
                           define =1? dup 1 = end 
                           define -- 1 - end 
                           define fib 
                           =0? if drop 0 exit endif 
                           =1? if drop 1 exit endif 
                           -- dup 
                           -- fib 
                           swap fib 
                           + 
                           end 
                           define make-fib 
                           dup 0 < if drop exit endif 
                           dup fib 
                           swap -- 
                           make-fib 
                           end 
                           10 make-fib     ))
              '((=0? (dup 0 =))
                 (=1? (dup 1 =))
                 (-- (1 -))
                 (fib (=0? (if (drop 0 exit)) =1? (if (drop 1 exit)) -- dup -- fib swap fib +))
                 (make-fib (dup 0 < (if (drop exit)) dup fib swap -- make-fib))
                10 make-fib))
        (test (parse #(   define =0? dup 0 = end 
                           define <0? dup 0 < end 
                           define signum 
                           =0? if exit endif 
                           <0? if drop -1 exit endif 
                           drop 
                           1 
                           end 
                           0 signum 
                           -5 signum 
                           10 signum       ))
              '((=0? (dup 0 =)) (<0? (dup 0 <)) (signum (=0? (if (exit)) <0? (if (drop -1 exit)) drop 1)) 0 signum -5 signum 10 signum))
        (test (parse #(   define abs 
                           dup 0 < 
                           if neg endif 
                           end 
                           9 abs 
                           -9 abs      ))
              '((abs (dup 0 < (if (neg)))) 9 abs -9 abs))
        (test (parse #(define word w1 w2 w3)) '#f)
        ))
;
(run-tests the-tests)