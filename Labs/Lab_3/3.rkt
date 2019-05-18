(define tribm
  (let ((memo '()))
    (lambda (n)
      (let ((memoized (assq n memo)))
        (if memoized
            (cadr memoized)
            (let ((new-value
                   (if (< n 3)
                       (if (= n 2)
                           1
                           0)
                       (+ (tribm (- n 1)) (tribm (- n 2)) (tribm (- n 3))))))
              (set! memo (cons (list n new-value) memo))
              new-value))))))
(tribm 50000)
(tribm 50000)