(load "assert.rkt")
(use-assertions)

; Простейшие

(define (1/x x)
  (assert (not (zero? x)))
  (/ 1 x))

(write (map 1/x '(-2 -1 0 1 2)))(newline)
(write (map 1/x '(1 2 3 4 5)))(newline)

; Выход из вложенных процедур

(define (my-reverse xs)
  (define (helper ys zs)
    (assert (list? ys))
    (if (null? ys)
        zs
        (helper (cdr ys)
                (cons (car ys) zs))))
  (assert (list? xs))
  (helper xs '()))

(define (reverse-string s)
  (assert (string? s))
  (list->string (my-reverse (string->list s))))

(write (reverse-string "abcdef")) (newline)
(write (reverse-string 1234)) (newline)
(write (my-reverse '(a b . c))) (newline)

; Выход из взаимной рекурсии

(define (my-even? n)
  (if (= n 0) #t (my-odd? (- n 1))))

(define (my-odd? n)
  (assert (> n 0))
  (if (= n 0) #f (my-even? (- n 1))))

(write (my-even? 10)) (newline)
(write (my-odd? 5)) (newline)
(write (my-even? -5)) (newline)