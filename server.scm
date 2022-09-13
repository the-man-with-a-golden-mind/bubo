(define *path* (cons "./libs" *path*))
(define-library (bubo server)
  
  (import (otus lisp))
  (import (lib http))
  (import (lang macro))
  (import (owl parse))
  (import (parsers parsers))

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
   
    (define (get-body parser text content-length)
       (let ((forced-list (ltake text (string->number content-length))))
        (if (< (length forced-list) 1)
          #false
          (car (try-parse parser forced-list #false)))))

    (define (send-file path fd send)
      ;(print "SEND FILE")
      (define stat (syscall 4 (if (string? path) (c-string path))))
      ;(print "STAT:" stat)
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

    (define (redirect content send)
      (send "HTTP/1.0 307\n"
        (string-append "Location: " content "\n")
        conn-close
        server-version
        "\n\n"))

    (define (request-dispatch req-type content request)
      ;(print request)
      (let* ((fd (get request 'fd #false))
             (send (get request 'send #false)))
       (cond
      ; static files
      ((eq? req-type 'static)
        (send-file content fd send))
      ; JSON API Responses
      ((eq? req-type 'api)
        (send-api content fd send))                                  
      ; templates
      ((eq? req-type 'template)
        (send-template content send))
      ((eq? req-type 'redirect)
        (redirect content send))
      (else 
       (send-template content send))
      )))

    (define (logo)
      "
    ~\\___/~
     (0 0)
     ) V (
      ! !

   HOO! HOO!

      "
     )

    (define (server-start port router) 
      (print (logo))
      (print "Bubo is owl-ing...")
      (http:run port (lambda (fd request headers body close)
                      (define (send . args)
                        (for-each (lambda (arg)
                                    (display-to fd arg)) args))
                 (let* ((content-length (get headers 'content-length "0"))
                        (stringed-body (get-body get-string body content-length))
                        (request-compact 
                          (make-ff 
                            (list 
                              'body stringed-body
                              'path request
                              'headers headers
                              'fd fd
                              'send send)))) 
                  (router request-compact))
                 (close #t))))))
