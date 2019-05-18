(define (my-string-append . strings)
  (list->string
   (apply append
          (map string->list strings))))