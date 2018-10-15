(defpackage #:let-over-lambda
  (:nicknames #:lol)
  (:use #:cl #:cl-user #:cl-ppcre)
  (:import-from #:alexandria
                #:parse-body)
  (:import-from #:named-readtables
                #:defreadtable
                #:in-readtable)
  (:export #:lol-syntax
           #:in-readtable
           #:mkstr
           #:symb
           #:group
           #:flatten
           #:fact
           #:choose
           #:g!-symbol-p
           #:defmacro/g!
           #:o!-symbol-p
           #:o!-symbol-to-g!-symbol
           #:defmacro!
           #:defun!
           #:|#"-reader|
           #:segment-reader
           #:match-mode-ppcre-lambda-form
           #:subst-mode-ppcre-lambda-form
           #:|#~-reader|
           #:dlambda
           #:alambda
           #:aif
           #:|#`-reader|
           #:|#f-reader|
           #:nlet-tail
           #:alet%
           #:alet
           #:it
           #:this
           #:self
           #:let-binding-transform
           #:pandoriclet
           #:pandoriclet-get
           #:pandoriclet-set
           #:get-pandoric
           #:with-pandoric
           #:pandoric-hotpatch
           #:pandoric-recode
           #:plambda
           #:pandoric-eval
           #:fast-progn
           #:safe-progn
           #:fformat
           #:make-tlist
           #:tlist-left
           #:tlist-right
           #:tlist-empty-p
           #:tlist-add-left
           #:tlist-add-right
           #:tlist-rem-left
           #:tlist-update
           #:build-batcher-sn
           #:sortf
           #:dollar-symbol-p
           #:prune-if-match-bodies-from-sub-lexical-scope
           #:if-match
           #:when-match
           #:nif))

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
           :with-multiple-value-bindings
           :keyw))

(defpackage :parser
  (:use :cl :lol :utils)
  (:export :tuple :result :apply-parser :parse :bind :fail :item :do-with :sat :sym :choose :choose-among :zero :plus :choice 
           :many :many1 :sepby :sepby1 :sublist :pair :optional :atomic :var-init :req-var :opt-var :lambda-list :arg-names))

(defpackage :grammar
  (:use :cl :lol :utils)
  (:export :defprim :defprod
           :synth :synth-all :synth-plist :synth-plist-merge))

(defpackage :msim
  (:use :cl :lol :utils :parser :grammar ))



