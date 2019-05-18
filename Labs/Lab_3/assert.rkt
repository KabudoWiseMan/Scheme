(define global-escape #f)

(define (use-assertions)
  (call-with-current-continuation
   (lambda (escape)
     (set! global-escape escape))))

(define (assert% cond-value cond-expr)
  (if (not cond-value)
      (begin
        (newline)
        (display "FAILED: ")
        (write cond-expr)
        (newline)
        (global-escape))))

(define-syntax assert
  (syntax-rules ()
    ((_ expr) (assert% expr (quote expr)))))