(asdf:defsystem :msim
  :serial t
  :description "Msim market simulator"
  :author "Giuseppe Viola gius.viola@gmail.com"
  :license ""
  :components ((:file "package")
               (:file "lol")
               (:file "parser")
               (:file "grammar") 
               (:file "utils")
               (:file "iter")
               (:file "msim")))
