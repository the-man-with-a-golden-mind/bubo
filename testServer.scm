(define *path* (cons "./libs" (cons "../" *path*)))

(import (bubo server))
(import (bubo match))
(import (shot template))
(define port 8081)

(define (router request)
  (print request)
  (let* ((path (get request 'path "/"))
         (splitted-route (route path)))
    (print "OK")
    (CASE splitted-route
          (MATCH ("GET" "images" string?) =>
                 (r-type images filename)
                 (print "FILENAME: " filename)
                 (request-dispatch 'static (string-append "./" images "/" filename) request))
          (MATCH ("GET" string?) =>
                 (path name)
                 (request-dispatch 'template 
                      (IMG 
                        (list 
                         (list "src" (string-append "http://localhost:" (number->string port) "/images/buboMeme.jpeg"))) "")
                    request))
          (else
           (request-dispatch 'template (DIV '() (H1 '() "TEST")) request)))))


(server-start port router)
