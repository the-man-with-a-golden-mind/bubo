
(define-library (bubo server)
  (import (otus lisp))
  (import (lib http))
  (import (lang macro))

  (export 
   server-start
   request-dispatch
   )
  (begin
    (define server-version "Server: Bubo / 0.01 \n")

    (define ok-200 "HTTP/1.0 200 OK\n")
    (define err-500 "HTTP/1.0 500 Internal Server Error\n")
    (define err-404 "HTTP/1.0 404 Not Found\n")

    (define conn-close "Connection: close\n")
    (define content-html "Content-Type: text/html; charset=UTF-8\n")
    (define content-json "Content-Type: application/json")
    (define content-stream "Content-Type: application/octet-stream\n")
   

    (define (send-file path fd send)
      (print "SEND FILE")
      (define stat (syscall 4 (if (string? path) (c-string path))))
      (print "STAT:" stat)
      (if stat
          (cond
                                            ; regular file?
           ((not (zero? (band (ref stat 3) #o0100000)))
            (define file (open-input-file path))
            (if file
                then
                (begin (send ok-200
                             conn-close
                             content-stream
                             "Content-Length: " (ref stat 8) "\n"
                             server-version
                            "\n")
                       (syscall 40 fd file 0 (ref stat 8)) ; sendfile
                    (close-port file))
                else
                (send err-500
                      conn-close
                      "\n")))
           (else
            (send err-500
                  conn-close
                  "\n"))))
      (send err-500
            conn-close
            "\n"))
            
    (define (send-template template send)
      (send ok-200
            conn-close
            content-html
            server-version
            "\n\n"
            template))

    (define (send-api content send)
      (send ok-200
       conn-close
       content-json
       server-version
       "\n\n"
       content))

    (define (request-dispatch req-type content fd send)
      (cond
      ; static files
      ((eq? req-type 'static)
        (send-file content fd send))
      ; JSON API Responses
      ((eq? req-type 'api)
        (send-api content fd send))                                  
      ; templates
      (else
        (send-template content send))))


    (define (server-start port router)
      (http:run port (lambda (fd request headers body close)
                      (define (send . args)
                        ;(print "ARGS:" args)
                        (for-each (lambda (arg)
                                    (display-to fd arg)) args))
                 ;(print "FD:" fd)
                 ;(print "REQUEST:" (route request))
                 ;(print "BODY:" body)
                 ;(print ":: " (syscall 51 fd))
                 (router fd send request)
                 (close #t)))
     )
   )
 )
