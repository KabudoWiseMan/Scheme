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
(define spaces (string->list " \t\n\r\v\f"))
;
(define (tokenize str)
  (define src (make-source str #\null))
  (define (end? char) (and (eq? char (vector-ref src 3)) (= (vector-ref src 2) (vector-ref src 1))))
  (define (sign? char) (or (eq? char #\-)
                           (eq? char #\+)
                           (eq? char #\*)
                           (eq? char #\/)
                           (eq? char #\^)))
  (call-with-current-continuation
   (lambda (fail)
     (begin
       (define (helper substring final-list)
         (let ((ch (next! src)))
           (cond ((end? ch) (append final-list
                                    (if (and (null? substring) substring)
                                        substring
                                        (list (or
                                               (and
                                                (char-numeric? (car substring))
                                                (string->number (list->string substring)))
                                               (string->symbol (list->string substring)))))))
                 ((memq ch spaces) (helper '() (append final-list
                                                       (if (and (null? substring) substring)
                                                           substring
                                                           (list (or
                                                                  (and
                                                                   (char-numeric? (car substring))
                                                                   (string->number (list->string substring)))
                                                                  (string->symbol (list->string substring))))))))
                 ((sign? ch) (helper '() (helper '() (append final-list
                                                             (if (and (null? substring) substring)
                                                                 substring
                                                                 (list (or
                                                                        (and
                                                                         (char-numeric? (car substring))
                                                                         (string->number (list->string substring)))
                                                                        (string->symbol (list->string substring)))))
                                                             (list (string->symbol (string ch)))))))
                 ((char-numeric? ch) (helper (append substring (list ch)) final-list))
                 ((or (eq? ch '#\() (eq? ch '#\))) (helper '() (append final-list
                                                                       (if (and (null? substring) substring)
                                                                           substring
                                                                           (list (or
                                                                                  (and
                                                                                   (char-numeric? (car substring))
                                                                                   (string->number (list->string substring)))
                                                                                  (string->symbol (list->string substring)))))
                                                                       (list (string ch)))))
                 ((and (>= (char->integer ch) 97) (<= (char->integer ch) 122))
                  (helper (append substring (list ch)) final-list))
                 (else (fail #f)))))
         (helper '() '())))))
;
(define (parse program)
  (define (helper program final-program)
    (cond ((null? program) final-program)
          ((symbol? (car program)) 
  (helper program '()))