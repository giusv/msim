(asdf:defsystem :msim
  :serial t
  :description "Msim market simulator"
  :author "Giuseppe Viola gius.viola@gmail.com"
  :license ""
  :depends-on (#:alexandria
               #:cl-ppcre
               #:named-readtables
               ;; #:cffi
               ;; #:clsql
               ;; #:drakma
               )
  :components ((:file "package")
               (:file "lol")
               (:file "utils")
               (:file "parser")
               (:file "grammar") 
               (:file "iter")
               (:file "compiler")
               ;; (:file "msim")
               ;; (:file "dbtest")
               ;; (:file "clostest")
               ))
