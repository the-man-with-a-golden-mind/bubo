(define *path* (cons "./libs" *path*))
(define-library (bubo db)
  (import (otus lisp))
  (import (cmd command))
  (import (lib sqlite))
  (import (parsers parsers))
  (import (scheme file))

  (export 
    init-db
    get-result-as-row
    conn-db
    rows
    database
    migrate)

  (begin
    (define database (make-sqlite3))

    ; Function will get the DB details, connect to the DB using DB-Conn, remove everything from the DB and run DB create script
    (define (init-db db-handler)
     ; Check if there are migrations TABLE in the db
     (let* ((table-names (sqlite:query db-handler "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name;"))
            (are-migrations-there (member "migrations" table-names)))
      (if (not are-migrations-there)
        (begin
          (print "Creating migration table!")
          (sqlite:query db-handler "CREATE TABLE IF NOT EXISTS \"migrations\" (
            \"id\" INTEGER PRIMARY KEY AUTOINCREMENT,
            \"created_at\" timestamp DEFAULT CURRENT_TIMESTAMP,
            \"file_name\" STRING NOT NULL);")
       ))))
  
      (define (get-result-as-row statement)
      (let ((n (sqlite3_column_count statement)))
         (if (less? 0 n)
            (let subloop ((i (- n 1)) (args '()))
               (if (< i 0) args
                  (subloop (- i 1) (cons
                     (case (sqlite3_column_type statement i)
                        (SQLITE_NULL    #false) ; should be #false or #null?
                        (SQLITE_INTEGER (sqlite3_column_int statement i))
                        (SQLITE_FLOAT   (sqlite3_column_double statement i))
                        (SQLITE_TEXT    (sqlite3_column_text statement i))
                        (SQLITE_BLOB    (syscall 9 ; mmap
                           (sqlite3_column_blob statement i)
                           (sqlite3_column_bytes statement i) 0))
                        (else (error "Unsupported column type " i)))
                     args)))))))
    
    (define (to-rows db-result to-vector?)
      (case db-result
         (#f #false) ; error
         (#t #null)  ; no data available
         (else
            (define result 
              (reverse
                (let loop ((t '()))
                  (let ((row (get-result-as-row db-result)))
                    (if row
                      (case (sqlite3_step db-result)
                        (SQLITE_ROW
                          (loop (cons row t)))
                        (SQLITE_DONE
                          (sqlite3_finalize db-result)
                          (cons row t))
                      (else
                        (sqlite3_finalize db-result)
                        (error "Can't execute SQL query"))))))))
            (if to-vector?
              (list->vector (map list->vector result))
              result))))

    (define rows
      (case-lambda
        ((db-result) (to-rows db-result #false))
        ((db-result to-vector?) (to-rows db-result #true))))

    ; Function will get the DB details and return the DB handler
    (define (conn-db database-handler db-conn-details)
     (let ((db-path (get db-conn-details 'path ":memory:")))
      (print "Connecting into database: " db-path)
      (sqlite3_open db-path database-handler)
      (display " ...Connected!\n")))
    
    (define (insert-migration db-handler filename)
      (sqlite:query db-handler "INSERT INTO migrations (file_name) VALUES (?);" filename))

    ; Function is reading file and running on the DB-HANDLER
    (define (run-migration db-handler filepath filename)
     (print "FILENAME: " filename "\n")
      (let* ((file (open-input-file (string-append filepath "/" filename)))
             (content (parse-string file)))
        (begin 
          (print "MIGRATION CONTENT: \n" content)
          (close-port file)
          (if content
            (let* ((migration-result (sqlite:query db-handler content))
                   (migration-result-rows (rows migration-result)))
              (if migration-result
                (insert-migration db-handler filename)
                (print "Can not make migraiton from file " filename)))
            (print "Can not open and process migration: " filename)))))

    (define (make-migrations db-handler migrations-path)
      (let* ((ls-path (or (get-environment-variable "LS_DIR") "/bin/ls"))
              (migrations (make-cmd ls-path (list "ls" "-1" migrations-path) get-lines)))
      (if (< (size migrations) 1)
        (print "Can not find migrations at " migrations-path)
        (let* ((sorted-migrations (sort (lambda (a b) (string<? (car a) (car b))) migrations))
               (migrations-from-db (sqlite:query db-handler "select * from MIGRATIONS order by file_name DESC"))
               (migrations-db-rows (rows migrations-from-db))
               (migrations-names (map (lambda (elem)  (list-ref elem 2)) migrations-db-rows))
               (filtered-migrations (filter (lambda (elem) (not (member (car elem) migrations-names))) sorted-migrations)))
         
         (if (> (length filtered-migrations) 0)
          (map (lambda (elem) (run-migration db-handler migrations-path elem)) (car filtered-migrations))
          '()) 
         ))))

    ; Function will get all migrations from files and database. Compare them and run migrations which are not in the DB. 
    (define migrate
     (case-lambda
      ((db-handler migrations-path) (make-migrations db-handler migrations-path))
      ((db-handler) (make-migrations db-handler "./migrations"))))
   ))
