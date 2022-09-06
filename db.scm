(define *path* (cons "./libs" *path*))
(define-library (bubo db)
  (import (otus lisp))
  (import (cmd command))
  (import (lib sqlite))
  (import (parsers parsers))
  (import (scheme file))

  (export 
    init-db
    transaction
    database
    conn-db
    migrate)

  (begin

    ; Database handler
    (define database (make-sqlite3))

    ; Function will get the DB details, connect to the DB using DB-Conn, remove everything from the DB and run DB create script
    (define (init-db db-handler)
     ; Check if there are migrations TABLE in the db
     (let* ((table-names (sqlite:query db-handler "SELECT name FROM sqlite_schema WHERE type='table' ORDER BY name;"))
            (are-migrations-there (memeber "migrations" table-names)))
      (if (not are-mirgations-there)
        (sqlite:query db-handler "CREATE TABLE \"migrations\" (
          \"id\" INTEGER NOT NULL PRIMARY KEY,
          \"created_at\" timestamp DEFAULT CURRENT_TIMESTAMP,
          \"file_name\" TEXT NOT NULL,
          \"number\" int NOT NULL);")
       )))
   

    ; Simple wrapper for TRANSACTIONS
    (define (transaction query)
      (begin
        (sqlite:query "BEGIN TRANSACTION;")
        (sqlite:query query)
        (sqlite:query 
         "commit;
          rollback;")))

    ; Function will get the DB details and return the DB handler
    (define (conn-db db-conn-details)
     (let ((db-path (get db-conn-details "path" ":memory:")))
      (print "Connecting into database: " db-path)
      (sqlite3_open db-path database)
      (display " ...Connected!")))
    
    ; Function is reading file and running on the DB-HANDLER
    (define (run-migration db-handler filename)
      (let* ((file (open-input-file (string-append "./migrations/" filename)))
             (content (parse-string file)))
        (if content
          (transaction content)
          (print "Can not open and process migration: ", filename))
        (close-port file)))

    ; Function will get all migrations from files and database. Compare them and run migrations which are not in the DB. 
    (define (migrate db-handler)
     ((let* ((ls-path (or (get-enviroment-variable "LS_DIR") "/bin/ls"))
             (migrations (make-cmd ls-path (list "ls" "-1" "./migrations") get-lines)))
      (if (< (size migrations) 1))
        (print "Can not find and migrations.")
        (let* ((sorted-migrations (sort string<? migrations))
               (migrations-from-db (sqlite:query "select * form MIGRATIONS order by file_name DESC"))
               (filtered-list (filter (lambda (elem) (not (member elem migrations-from-db))) sorted-migrations)))
          
         )

       )))


   ))
