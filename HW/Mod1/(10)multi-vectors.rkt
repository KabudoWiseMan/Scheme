(define (make-multi-vector1 sizes)
  (if (null? sizes)
      0
      (make-vector (car sizes) (make-multi-vector1 (cdr sizes)))))
;
(define (make-multi-vector2 sizes fill)
  (if (null? sizes)
      fill
      (make-vector (car sizes) (make-multi-vector2 (cdr sizes) fill))))
;
(define (make-multi-vector . xss)
  (if (= (length xss) 1)
      (make-multi-vector1 (car xss))
      (make-multi-vector2 (car xss) (cadr xss))))
;
(define (multi-vector? m)
  (and (vector? m) (vector? (vector-ref m 0))))
;
(define (multi-vector-ref m indices)
  (if (null? (cdr indices))
      (vector-ref m (car indices))
      (multi-vector-ref (vector-ref m (car indices)) (cdr indices))))
;
(define (multi-vector-set! m indices x)
  (if (null? (cdr indices))
      (vector-set! m (car indices) x)
      (multi-vector-set! (vector-ref m (car indices)) (cdr indices) x)))