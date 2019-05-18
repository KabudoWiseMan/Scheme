(define-syntax reverse-order
  (syntax-rules ()
    ((_ e) (reverse-order e ()))
    ((_ (e . rest) r)
     (reverse-order rest (e . r)))
    ((_ () r) r)))