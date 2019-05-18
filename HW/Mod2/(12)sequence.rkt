(define (make-source sequence . end-symbol)
  (let*
      ((buffer
        (cond
          ((vector? sequence) (vector->list sequence))
          ((list? sequence) sequence)
          ((string? sequence) (string->list sequence))))
       (size (length buffer))
       (end (and (not (null? end-symbol)) (car end-symbol))))
    (list buffer size 0 end)))
;
(define (peek sequence)
  (let ((seq (car sequence))
        (len (length (car sequence)))
        (pos 0)
        (end (list-ref sequence 3)))
    (if (< pos len)
        (list-ref seq pos)
        end)))
;
(define-syntax next
  (syntax-rules ()
    ((_ sequence) (let ((seq (car sequence))
                        (len (length (car sequence)))
                        (pos 0)
                        (end (list-ref sequence 3)))
                    (if (< pos len)
                        (begin (set! sequence (make-source (cdar sequence) end))
                               (+ pos 1)
                               (list-ref seq pos))
                        end)))))