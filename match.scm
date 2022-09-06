(define-library (bubo match)
  (import (otus lisp))
  (export CASE M flatten route)
  (begin

    (define (flatten x)
      (cond
       ((null? x)
        '())
       ((not (pair? x))
        (list x))
       (else
        (append (flatten (car x))
                (flatten (cdr x))))))

    (define (route path)
      (let* ((http-method (vector-ref path 0))
             (http-path (vector-ref path 1))
             (http-path-splitted (c/\// http-path)))
        (flatten `(,http-method ,(cdr http-path-splitted)))))


  ; this function makes pattern mathing:
    (define (M pattern items)
      (when (= (length pattern) (length items))
        (fold (lambda (f a b)
                (and f (cond
                        ((string? a)
                         (string-eq? a b))
                        ((number? a)
                         (= a b))
                        ((function? a)
                         (a b))
                        (else
                         #false))))
              #true
              pattern
              items)))

  ; just return true
    (define (any .args) #true)

  ; splits string onto parts, makes part a number if string is a number
    (define (convert path)
      (map (lambda (item)
             (cond
              ((m/[0-9]+/ item)
               (string->number item))
              (else
               item)))
           (cdr (c/\// path))))

  ; heart of matcher - a macro
    (define-syntax CASE
      (syntax-rules (MATCH => else)
        ((CASE thing) #false)
        ((CASE thing (else exp . rest))
         (begin exp . rest))
        ((CASE thing (MATCH (pattern...) => (args...) .body) .rest)
         (if (M (list pattern...) thing)
             (apply
              (lambda (args...) .body) thing)
             (CASE thing . rest)))))))
