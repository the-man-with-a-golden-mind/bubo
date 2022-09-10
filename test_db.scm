(define *path* (cons "./libs" (cons "../" *path*)))

(import (bubo db))
(import (parsers parsers))
(import (otus lisp))
(import (lib sqlite))
;(import (lib rlutil))

(define GREEN "\e[0;32m")
(define YELLOW "\e[0;33m")
(define MAGENTA "\e[0;35m")
(define END "\e[0;0m")

(define (test-init-db)
  (print END "Testing the DB initialization...")
  (define db-handler database)
  (begin
   ; Connection to db in the memory
   (conn-db db-handler #empty)
   (init-db db-handler)
   (let* ((result (sqlite:value db-handler "SELECT name FROM sqlite_master WHERE type='table' ORDER by name;'"))) 
    (display MAGENTA)
    (assert result ===> "migrations")
    (print GREEN "...ok \n"))))

(define (test-rows)
  (define db-handler database)
  (begin
    (print END "Testing returning ROWS from DB...")
    (conn-db db-handler #empty)
    (init-db db-handler)
    (sqlite:query db-handler "CREATE TABLE testRows (ID INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, NAME TEXT NOT NULL);")
    (sqlite:query db-handler "INSERT INTO testRows (name) VALUES (\"Michal\");")
    (sqlite:query db-handler "INSERT INTO testRows (name) VALUES (\"Henryk\");")
    (sqlite:query db-handler "INSERT INTO testRows (name) VALUES (\"Dominik\");")
    (let* ((result (sqlite:query db-handler "SELECT * FROM testRows where id > 0;"))
           (result-rows (rows result)))
     (display MAGENTA)
     (assert (length result-rows) ===> 3)
     (print GREEN "...ok\n"))))

(define (test-migrate)
  (print END "Testing DB migration system...")
  (define db-handler database)
  (begin
    (conn-db db-handler #empty)
    (init-db db-handler)
    (let* ((temp (migrate db-handler "./test_migrations"))
           (migrations-db (sqlite:query db-handler "SELECT name FROM sqlite_master where type='table' ORDER BY name;"))
           (migrations-rows (rows migrations-db)))
      (display MAGENTA)
      (assert migrations-rows  ===> (list (list "migrations") (list "sqlite_sequence") (list "testTable") (list "testTable2")))
      (print GREEN "...ok\n")
      )))

(define (test-migrate-no-dupes)
  (print END "Testing DB migration system - no dupes...")
  (define db-handler database)
  (begin
    (conn-db db-handler #empty)
    (init-db db-handler)
    (let* ((temp (migrate db-handler "./test_migrations"))
           (temp2 (migrate db-handler "./test_migrations"))
           (migrations-db (sqlite:query db-handler "SELECT name FROM sqlite_master where type='table' ORDER BY name;"))
           (migrations-rows (rows migrations-db)))
      (display MAGENTA)
      (assert migrations-rows  ===> (list (list "migrations") (list "sqlite_sequence") (list "testTable") (list "testTable2")))
      (print GREEN "...ok\n")
      )))

(define tests 
 (list
  test-rows
  test-migrate
  test-migrate-no-dupes
  test-init-db))

(map (lambda (test) (test)) tests)
