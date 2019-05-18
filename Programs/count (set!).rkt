(define count
  (let ((c 0))
    (lambda ()
      (begin (set! c (+ c 1))
             c))))