(define input-port
  (open-input-file "test.txt"))
(display (input-port? input-port))

(define (read-chars)
  (let ((ch (read-char input-port)))
    (if (eof-object? ch)
        '()
        (cons ch (read-chars)))))