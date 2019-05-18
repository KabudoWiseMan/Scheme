(define (save-data data path)
  (let ((file (open-output-file path)))
    (begin
      (write data file)
      (close-output-port file))))

(define (load-data path)
  (let* ((file (open-input-file path))
         (result (read file)))
    (begin
      (close-input-port file)
      result)))

(define (count-lines path)
  (define (inner file count)
    (let* ((c (read-char file)))
      (cond ((eof-object? c) count)
            ((equal? c #\newline) (inner file (+ count 1)))
            (else (inner file count)))))
  (let ((file (open-input-file path)))
    (inner file 1)))