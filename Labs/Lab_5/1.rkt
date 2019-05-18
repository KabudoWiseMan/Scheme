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
;
;
;<Final> ::= <Start1> <Start2> <Number> .
;<Number> ::= <Number> <Digit> | <Digit> .
;<Start1> ::= "0" .
;<Start2> ::= "x" | "X" .
;<Digit> ::= "0" | "1" | ... | "9" | "A" | "B" | ... | "F" | "a" | "b" | ... "f" .
(define (check-hex str)
  (define src (make-source str #\null))
  (define (end? char) (and (eq? char (vector-ref src 3)) (= (vector-ref src 2) (vector-ref src 1))))
  (define (start1? char) (and (eq? char #\0) (= (vector-ref src 2) 1)))
  (define (start2? char) (and (or (eq? char #\x) (eq? char #\X))
                              (= (vector-ref src 2) 2)))
  (define (digit? char) (and (> (vector-ref src 2) 2)
                             (or (and (>= (char->integer char) 48) (<= (char->integer char) 57))
                                  (and (>= (char->integer char) 65) (<= (char->integer char) 70))
                                  (and (>= (char->integer char) 97) (<= (char->integer char) 102)))))
  (call-with-current-continuation
   (lambda (fail)
     (begin
       (define (continue-scan)
         (let ((ch (next! src)))
           (cond
             ((end? ch) #t)
             ((= (vector-ref src 1) 2) #f)
             ((start1? ch) (continue-scan))
             ((start2? ch) (continue-scan))
             ((digit? ch) (continue-scan))
             (else (fail #f)))))
       (continue-scan)))))
;
(define (scan-hex str)
  (define src (make-source str #\null))
  (define (end? char) (and (eq? char (vector-ref src 3)) (= (vector-ref src 2) (vector-ref src 1))))
  (define (start1? char) (and (eq? char #\0) (= (vector-ref src 2) 1)))
  (define (start2? char) (and (or (eq? char #\x) (eq? char #\X))
                              (= (vector-ref src 2) 2)))
  (define (digit? char) (and (> (vector-ref src 2) 2)
                             (or (and (>= (char->integer char) 48) (<= (char->integer char) 57))
                                  (and (>= (char->integer char) 65) (<= (char->integer char) 70))
                                  (and (>= (char->integer char) 97) (<= (char->integer char) 102)))))
  (call-with-current-continuation
   (lambda (fail)
     (begin
       (define (continue-scan)
         (let ((ch (next! src)))
           (cond
             ((end? ch) 0)
             ((= (vector-ref src 1) 2) #f)
             ((start1? ch) (continue-scan))
             ((start2? ch) (continue-scan))
             ((digit? ch) (or (and
                               (and (>= (char->integer ch) 48)
                                    (<= (char->integer ch) 57))
                               (+ (* (- (char->integer ch) 48) (expt 16 (- (vector-ref src 1) (vector-ref src 2)))) (continue-scan)))
                              (and
                               (and (>= (char->integer ch) 65)
                                    (<= (char->integer ch) 70))
                               (+ (* (- (char->integer ch) 55) (expt 16 (- (vector-ref src 1) (vector-ref src 2)))) (continue-scan)))
                              (and
                               (and (>= (char->integer ch) 97)
                                    (<= (char->integer ch) 102))
                               (+ (* (- (char->integer ch) 87) (expt 16 (- (vector-ref src 1) (vector-ref src 2)))) (continue-scan)))))
             (else (fail #f)))))
       (continue-scan)))))
;
(define spaces (string->list " \t\n\r\v\f"))
;
(define (scan-many-hexs str)
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
        (check-hex (list->string x))
        (append l (list (scan-hex (list->string x))))))
      ((memq (car str) spaces) 
       (and
        (check-hex (list->string x))
        (helper1 (cdr str) (append l (list (scan-hex (list->string x)))))))
      (else (helper2 (cdr str) l (append x (list (car str)))))))
  (helper1 (string->list str) '()))
;
;tests
;
(load "unit-testing.rkt")
(define the-tests
  (list (test (check-hex "0x1") '#t)
        (test (check-hex "0X2A") '#t)
        (test (check-hex "0XFFF") '#t)
        (test (check-hex "0X025") '#t)
        (test (check-hex "123") '#f)
        (test (check-hex "0x") '#f)
        ;
        (test (scan-hex "0x0") '0)
        (test (scan-hex "0X10A") '266)
        (test (scan-hex "0x1") '1)
        (test (scan-hex "12") '#f)
        ;
        (test (scan-many-hexs "\t0x1\t\t0x2\n0xff00") '(1 2 65280))
        (test (scan-many-hexs "0x0, 123") '#f)
        ))
;
(run-tests the-tests)