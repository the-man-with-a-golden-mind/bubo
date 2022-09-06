(define *path* (cons "./libs" (cons "../" *path*)))

(import (bubo server))
(import (bubo match))
(import (shot template))

(define port 8081)

(define (router fd send  path)
  (let* ((splitted-route (route path)))
    (CASE splitted-route
          (MATCH ("GET" "images" string?) =>
                 (r-type images filename)
                 (print "FILENAME: " filename)
                 (request-dispatch 'static (string-append "./" images "/" filename) fd send))
          (MATCH ("GET" string?) =>
                 (path name)
                 (request-dispatch 'template 
                      (IMG 
                        (list 
                         (list "src" (string-append "http://localhost:" (number->string port) "/images/buboMeme.jpeg"))) "")
                    #false send))
          (else
           (request-dispatch 'template (DIV '() (H1 '() "TEST")) #false send)))))


(server-start port router)
