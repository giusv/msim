;; (pprint (clsql:connect '("/giusv/temp/eurusd.db") :database-type :sqlite3))

;; (clsql:disconnect)

(clsql:with-database (db  '("/giusv/temp/eurusd.db") :database-type :sqlite3)
  (pprint (clsql:database-name db))
  (time (let ((v (clsql:query "select test from float" :database db :flatp t)))
     (pprint "ok")
     (pprint (reduce #'+ v :initial-value 0.0d0)))))
;; (clsql:enable-sql-reader-syntax)
;; (clsql:with-database (db  '("/giusv/temp/eurusd.db") :database-type :sqlite3)
;;   (clsql:set-autocommit nil :database db) 
;;   (dotimes (i 100)
;;     (clsql:insert-records :into '[float] :attributes '(test) :values `(,(exp (/ i 100))) :database db))
;;   (clsql:commit :database db))

;; (time (clsql:with-database (db  '("/giusv/temp/eurusd.db") :database-type :sqlite3)
;;         (clsql:with-transaction (:database db)
;;           (dotimes (i 1000000)
;;             (clsql:execute-command (format nil "insert into float(test) values(~a)" (* i 0.124236)) :database db)))))



