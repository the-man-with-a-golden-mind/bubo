(define *path* (cons "./libs" (cons "../" *path*)))

(import (bubo server))
(import (otus lisp))
;(import (lib rlutil))

(define GREEN "\e[0;32m")
(define YELLOW "\e[0;33m")
(define MAGENTA "\e[0;35m")
(define END "\e[0;0m")

(define (test-querystring)
  (print END "Testing querystring...")
  (define test-string "a=123&b=321&c=543")
  (let* ((result (querystring test-string))
         (a (car (get result 'a '()))))
   (display MAGENTA)
   (assert a ===> "123")
   (print GREEN "...ok\n")
   )
 )

(define tests
 (list test-querystring))

(map (lambda (test) (test)) tests)
