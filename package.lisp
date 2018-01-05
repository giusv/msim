(defpackage :lol
  (:use :cl)
  (:export :this :it
           :aif 
           :pandoriclet :get-pandoric  :dlambda :flatten :group :mkstr :symb :keyw))

(defpackage :utils
  (:use :cl :lol)
  (:export :random-number :random-string :random-boolean
           :rest-key :rest-plain
           :plist-keys :plist-values
           :plist-p
           :singular
           :bindall
           :glue
           :lower
           :lower-camel :upper-camel
           :split-str :interleave
           :append* 
           :write-file
           :my-debug
           :hash-table-keys
           :hash-table-values
           :overlaps
           :with-multiple-value-bindings))

(defpackage :parser
  (:use :cl :lol :utils)
  (:export :tuple :result :apply-parser :parse :bind :fail :item :do-with :sat :sym :choose :choose-among :zero :plus :choice 
           :many :many1 :sepby :sepby1 :sublist :pair :optional :atomic :var-init :req-var :opt-var :lambda-list :arg-names))

(defpackage :grammar
  (:use :cl :lol :utils)
  (:export :defprim :defprod
           :synth :synth-all :synth-plist :synth-plist-merge))

(defpackage :msim
  (:use :cl :lol))


