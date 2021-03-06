(in-package :msim)
(in-readtable lol-syntax)
(defparameter *buffer-size* 10)
(declaim (type fixnum *buffer-size*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun tactics-function (strat)
    (cond ((atom strat) strat)
          (t (apply (car strat) (mapcar #'tactics-function (cdr strat)))))))

(defprim price ()
  (:pretty () (list 'price))
  (:name () "PRICE")
  (:header (pref) (list (string-downcase (mkstr pref "-price"))))
  (:type () `((simple-array double-float (*)) fixnum))
  (:view (name)  `(,(synth :output this name 0)))
  (:init () (list '(make-array *buffer-size* :element-type 'double-float)
                  -1))
  (:state (name) (list (symb name "-PRICE")
                       (symb name "-START")))
  (:update (name price) `(setf
                          ,(symb name "-START") (the fixnum (mod (the fixnum (+ ,(symb name "-START") 1)) *buffer-size*))
                          (aref ,(symb name "-PRICE") ,(symb name "-START")) ,price))
  (:output (name i) `(aref ,(symb name "-PRICE") (the fixnum (mod (the fixnum (+ ,(symb name "-START") ,i)) *buffer-size*)))))

(defprim ema (alpha)
  (:pretty () (list 'ema (list :alpha alpha)))
  (:name () "EMA")
  (:header (pref) (list (string-downcase (mkstr pref "-ema"))))
  (:view (name)  (list (symb name "-VALUE")))
  (:init () (list 0.0d0
                  '(make-array *buffer-size* :element-type 'double-float)
                  -1))
  (:state (name) (list (symb name "-VALUE")
                       (symb name "-EMA")
                       (symb name "-START")))
  (:type () `(double-float (simple-array double-float (*)) fixnum))
  (:update (name price) `(setf
                          ,(symb name "-VALUE") (the double-float (+ (* ,alpha ,price) (the double-float (* ,(- 1.0d0 alpha) ,(symb name "-VALUE")))))
                          ,(symb name "-START") (the fixnum (mod (the fixnum (+ ,(symb name "-START") 1)) *buffer-size*))
                          (aref ,(symb name "-EMA") ,(symb name "-START")) ,(symb name "-VALUE")))
  (:output (name i) `(aref ,(symb name "-EMA") (mod (+ ,(symb name "-START") ,i) *buffer-size*))))

(defmacro defbasic (name pos) 
  `(defprim ,name () 
     (:pretty () (list ',name))
     (:name () (mkstr ',name))
     (:header (pref) (list (string-downcase (mkstr pref "-" ',name))))
     (:view (name) (declare (ignore name)) (list nil))
     (:init () nil)
     (:state (name) (declare (ignore name)) nil)
     (:type () nil)
     (:struct () nil)
     (:reset (name) (declare (ignore name))) 
     (:update (name price) (declare (ignore name price)) ,pos)
     (:output (name i) (declare (ignore name i)) ,pos)))
(defbasic bullish 1)
(defbasic bearish -1)
(defbasic neutral 0)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun cross-from-below (fast fast-name slow slow-name)
    `(and (> ,(synth :output fast fast-name 0) ,(synth :output slow slow-name 0))
          (<= ,(synth :output fast fast-name -1) ,(synth :output slow slow-name -1)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun cross-from-above (fast fast-name slow slow-name)
   `(and (< ,(synth :output fast fast-name 0) ,(synth :output slow slow-name 0))
         (>= ,(synth :output fast fast-name -1) ,(synth :output slow slow-name -1)))))

(defprim cross (fast slow bullish bearish)
  (:pretty () (list 'cross (list :fast (synth :pretty fast) :slow (synth :pretty slow)
                                 :bullish (synth :pretty bullish) :bearish (synth :pretty bearish))))
  (:name () "CROSS")
  (:header (pref)
           `(,@(synth :header fast (mkstr pref "-fast")) ,@(synth :header slow (mkstr pref "-slow"))
               ,(string-downcase (mkstr pref "-stance"))
               ,@(synth :header bullish (mkstr pref "-above" )) ,@(synth :header bearish (mkstr pref "-below"))))
  (:view (name) `(,@(synth :view fast (mkstr name "-FAST")) ,@(synth :view slow (mkstr name "-SLOW"))
                    ,(symb (mkstr name "-STANCE"))
                    ,@(synth :view bullish (mkstr name "-ABOVE" )) ,@(synth :view bearish (mkstr name "-BELOW"))))
  (:init () `(:init ,@(synth :init fast) ,@(synth :init slow)
                    ,@(synth :init bullish) ,@(synth :init bearish))) 
  (:state (name) (let ((fast-name (symb name "-FAST"))
                       (slow-name (symb name "-SLOW"))
                       (bullish-name (symb name "-BULLISH"))
                       (bearish-name (symb name "-BEARISH"))) 
                   `(,(symb name "-STANCE") ,@(synth :state fast fast-name) ,@(synth :state slow slow-name)
                      ,@(synth :state bullish bullish-name) ,@(synth :state bearish bearish-name))))
  (:type () `(symbol ,@(synth :type fast) ,@(synth :type slow)
                       ,@(synth :type bullish) ,@(synth :type bearish)))
  (:update (name price) 
           (let* ((stance-name (symb name "-STANCE"))
                  (fast-name (symb name "-FAST"))
                  (slow-name (symb name "-SLOW"))
                  (bullish-name (symb name "-BULLISH"))
                  (bearish-name (symb name "-BEARISH"))
                  ;; (fast-state (synth :state fast fast-name))
                  ;; (fast-value (symb name "-FAST-VALUE"))
                  ;; (slow-value (symb name "-SLOW-VALUE"))
                  ;; (slow-state (synth :state slow slow-name))
                  ;; (bullish-state (synth :state bullish bullish-name))
                  ;; (bearish-state (synth :state bearish bearish-name))
                  )
             `(fast-progn
               ,(synth :update fast fast-name price)
               ,(synth :update slow slow-name price)
               
               (case ,stance-name
                 (:init (cond (,(cross-from-below fast fast-name slow slow-name)
                               (progn (setf ,stance-name :above)
                                      ,(synth :update bearish bearish-name price)
                                      ,(synth :reset bullish bullish-name)
                                      ,(synth :update bullish bullish-name price)
                                      ))
                              (,(cross-from-above fast fast-name slow slow-name)
                               (progn (setf ,stance-name :below)
                                      ,(synth :update bullish bullish-name price)
                                      ,(synth :reset bearish bearish-name)
                                      ,(synth :update bearish bearish-name price)))
                              (t (progn ,(synth :update bullish bullish-name price)
                                        ,(synth :update bearish bearish-name price)
                                        0))
                              ))
                 (:above (cond (,(cross-from-above fast fast-name slow slow-name)
                                (progn (setf ,stance-name :below)
                                       ,(synth :update bullish bullish-name price)
                                       ,(synth :reset bearish bearish-name)
                                       ,(synth :update bearish bearish-name price)))
                               (t (progn ,(synth :update bearish bearish-name price)
                                         ,(synth :update bullish bullish-name price)))))
                 (:below (cond (,(cross-from-below fast fast-name slow slow-name)
                                (progn (setf ,stance-name :above)
                                       ,(synth :update bearish bearish-name price)
                                       ,(synth :reset bullish bullish-name)
                                       ,(synth :update bullish bullish-name price)
                                       ))
                               (t (progn ,(synth :update bullish bullish-name price)
                                         ,(synth :update bearish bearish-name price)))))))))
  (:reset (name) `(setf ,(symb name "-STANCE") :init)))




;; (let ((s (cross (price) (ema 0.1d0) 
;;                  (bullish)
;;                  (bearish))))
;;   (pprint (compile-struct-tactics (cross (price) (ema 0.1d0) 
;;                  (bullish)
;;                  (bearish)))))

;; (defmacro compile-tactics (strat)
;;   (let* ((strat (eval strat)))
;;     `(let ,(map 'list #2`(,a1 ,a2) (synth :state strat 'tactics) (synth :init strat))
;;        (dlambda
;;         (:update (price)
;;                  ,(synth :update strat 'tactics 'price))
;;         (:pretty () ',(synth :pretty strat))
;;         (:header ()
;;                  (list ,@(synth :header strat 'tactics)))
;;         (:view ()
;;                  (list ,@(synth :view strat 'tactics)))))))

;; (let* ((strat )) 
;;     (format t "~%~{~20@a~^|~}|~20@a~%" (funcall strat :header "") "position")
;;     (format t "~v@{~a~:*~}~%" 150 "=")
;;     )


;; (let ((s (cross (price) (ema 0.1d0) 
;;                           (bullish)
;;                           (bearish))))
;;   (pprint (synth :state s 'strategy)))

 ;; (with-tactics (state update)
 ;;  (cross (price) (ema 0.1d0) 
 ;;         (bullish)
 ;;         (bearish))
  
 ;;  )

;; (defun compile-tactics (tact)
;;   (let* ((tact (eval tact))
;;          (tact-name (symb (synth :name tact))))
;;     `(progn
;;        (makunbound ',tact-name)
;;        (defstruct ,tact-name
;;          ,@(map 'list (lambda (s i y) (list s i :type y))
;;                 (synth :state tact 'strategy)
;;                 (synth :init tact)
;;                 (synth :type tact)))
;;        (declaim (inline update))
;;        (defun update (state price)
;;          (declare (type ,tact-name state)
;;                   (type double-float price))
;;          (symbol-macrolet ,(mapcar #`(,a1 (,(symb tact-name "-" a1) state))
;;                                    (synth :state tact 'strategy)) 
;;            (values ,(synth :update tact 'strategy 'price) state))))))

(defun compile-tactics-to-lambda (tact)
  (let* ((tact (eval tact))
         (tact-name (symb (synth :name tact)))
         ;; (tact-name (symb (gensym (synth :name tact))))
         ;; (tact-name (symb (gensym)))
         ;; (tact-name (symb (synth :name tact) 676))
         )
    `(lambda (price)
       (declare (type (simple-array double-float (*)) price))
       (progn
         
         (defstruct ,tact-name
           ,@(map 'list (lambda (s i y) (list s i :type y))
                  (synth :state tact 'strategy)
                  (synth :init tact)
                  (synth :type tact)))
         ;; (declaim (inline update))
         (flet ((update (state price)
                  (declare (type ,tact-name state)
                           (type double-float price))
                  (symbol-macrolet ,(mapcar #`(,a1 (,(symb tact-name '- a1) state))
                                            (synth :state tact 'strategy)) 
                    (values ,(synth :update tact 'strategy 'price) state))))
           (declare (inline update))
           (let ((state (,(symb 'make- tact-name)))
                 (len (length price)))
             ;; (pprint state)
             (time (fast-progn
                    (loop for i fixnum from 0 to (the fixnum (- len 1)) do
                         (multiple-value-bind (p state)
                             (update state (the double-float (aref price i)))
                                        ;(pprint state)
                           ;; (pprint p)
                           state
                           p
                           ) 
                       ;; (format t "~{~20,8f~^|~}|~20,8f~%" (funcall s :state) pos)
                         )))
             ))
         ;; (unintern 'sdjkvahlkser)
        ))))
(pprint (compile-tactics-to-lambda (cross (price) (ema 0.1d0) 
                                   (bullish)
                                   (bearish))))
(defun simulate-tactics (tact price)
  (let* ((tact (eval tact))
         ;; (tact-name (symb (synth :name tact)))
         ;; (tact-name (gensym (mkstr (synth :name tact))))
         (simulate (compile nil
                            (compile-tactics-to-lambda tact))))
    (funcall simulate price)))

(let* ((len 1000000)
       (price (make-array len
                          :element-type 'double-float
                          :initial-contents (loop for i fixnum from 0 to (- len 1) collecting (sin (* 2.0 pi (/ i len)))))))
  (time (simulate-tactics (cross (price) (ema 0.1d0) 
                            (bullish)
                            (bearish)) price))
  ;; (simulate-tactics (cross (price) (ema 0.2d0) 
  ;;                          (bullish)
  ;;                          (bearish)) price)
  ;; (simulate-tactics (cross (price) (ema 0.3d0) 
  ;;                          (bullish)
  ;;                          (bearish)) price)
  )


;; (defmacro compile-struct-tactics (tact)
;;   (let* ((tact (tactics-function tact))
;;          (tact-name (symb (synth :name tact))))
;;     `(progn
;;        (makunbound ',tact-name)
;;        (defstruct ,tact-name
;;          ,@(map 'list (lambda (s i y) (list s i :type y))
;;                 (synth :state tact 'strategy)
;;                 (synth :init tact)
;;                 (synth :type tact)))
;;        (declaim (inline update))
;;        (defun update (state price)
;;             (declare (type ,tact-name state)
;;                      (type double-float price))
;;             (symbol-macrolet ,(mapcar #`(,a1 (,(symb tact-name "-" a1) state))
;;                                       (synth :state tact 'strategy)) 
;;               (values ,(synth :update tact 'strategy 'price) state))))))

;; (compile-struct-tactics (cross (price) (ema 0.1d0) 
;;                         (bullish)
;;                         (bearish)))
;; (let ((state (make-cross)))
;;   ;; (pprint state)
;;   (let* ((len 1000000)
;;          (price (make-array len
;;                             :element-type 'double-float
;;                             :initial-contents (loop for i fixnum from 0 to (- len 1) collecting (sin (* 2.0 pi (/ i len)))))))
    
;;     ;; (format t "~%~{~20@a~^|~}|~20@a~%" (funcall s :header "") "position")
;;     ;; (format t "~v@{~a~:*~}~%" 150 "=")
;;     (time (fast-progn
;;            (loop for i fixnum from 0 to (- len 1) do
;;                 (multiple-value-bind (p state)
;;                     (update state (the double-float (aref price i)))
;;                                         ;(pprint state)
;;                   ;; (pprint p)
;;                   state
;;                   p
;;                   ) 
;;               ;; (format t "~{~20,8f~^|~}|~20,8f~%" (funcall s :state) pos)
;;                 )))
;;     ))


;; (defun test ()
;;   (let* ((len 1000000)
;;          (price (make-array len
;;                             :element-type 'double-float
;;                             :initial-contents (loop for i fixnum from 0 to (- len 1) collecting (sin (* 2.0 pi (/ i len))))))
;;          (s
;;           (compile-tactics (cross (price) (ema 0.1d0) 
;;                           (bullish)
;;                           (bearish)))))
    
;;     ;; (format t "~%~{~20@a~^|~}|~20@a~%" (funcall s :header "") "position")
;;     ;; (format t "~v@{~a~:*~}~%" 150 "=")
;;     (declare (type function s))
;;     (time (fast-progn
;;            (loop for i fixnum from 0 to (- len 1) do
;;                 (let* ((curr-price (aref price i))) 
;;                   (funcall s :update curr-price) 
;;                   ;; (format t "~{~20,8f~^|~}|~20,8f~%" (funcall s :state) pos)
;;                   ))))))

;; (test)
;; (defun f (s)
;;   (values 1 (+ s 1)))
;; (defun test1 ()
;;   (let ((s 0))
;;     (dotimes (i 1000000)
;;       (multiple-value-bind (p s1) (f s)
;;         ;; (format t "~a: ~a~%" p s)
;;         (setf s s1)))))
;; (time (test1))

;; (defstruct s
;;   (f1 0.0d0 :type double-float)
;;   (f2 0.0d0 :type double-float)
;;   (f3 0.0d0 :type double-float)
;;   (f4 0.0d0 :type double-float)
;;   (f5 0.0d0 :type double-float)
;;   (f6 0.0d0 :type double-float)
;;   (f7 0.0d0 :type double-float)
;;   (f8 0.0d0 :type double-float)
;;   (f9 0.0d0 :type double-float)
;;   (f10 0.0d0 :type double-float))

;; (declaim (inline u))
;; (defun u (s)
;;   (fast-progn
;;    (setf (s-f1 s) (the double-float (+ (s-f1 s) 1.0d0))
;;          (s-f2 s) (the double-float (+ (s-f2 s) 2.0d0))
;;          (s-f3 s) (the double-float (+ (s-f3 s) 3.0d0))
;;          (s-f4 s) (the double-float (+ (s-f4 s) 4.0d0))
;;          (s-f5 s) (the double-float (+ (s-f5 s) 5.0d0))
;;          (s-f6 s) (the double-float (+ (s-f6 s) 6.0d0))
;;          (s-f7 s) (the double-float (+ (s-f7 s) 7.0d0))
;;          (s-f8 s) (the double-float (+ (s-f8 s) 8.0d0))
;;          (s-f9 s) (the double-float (+ (s-f9 s) 9.0d0))
;;          (s-f10 s) (the double-float (+ (s-f10 s) 10.0d0)))
;;    (values 1 s)))

;; (let ((s (make-s :f1 0.0d0 :f2 0.0d0
;;                  :f3 0.0d0 :f4 0.0d0
;;                  :f5 0.0d0 :f6 0.0d0
;;                  :f7 0.0d0 :f8 0.0d0
;;                  :f9 0.0d0 :f10 0.0d0)))
;;  ; (pprint s)
;;   (time (dotimes (i 10)
;;           (multiple-value-bind (p s) (u s)
;;             (pprint s)
;;             p
;;             s
;;             ))))
;; (declaim (inline u1))
;; (defun u1 (f1 f2 f3 f4 f5 f6 f7 f8 f9 f10)
;;   (fast-progn
;;    (declare (type double-float f1)
;;             (type double-float f2)
;;             (type double-float f3)
;;             (type double-float f4)
;;             (type double-float f5)
;;             (type double-float f6)
;;             (type double-float f7)
;;             (type double-float f8)
;;             (type double-float f9)
;;             (type double-float f10)
;;             )
;;    (values 1
;;            (the double-float (+ f1 1.0d0))
;;            (the double-float (+ f2 2.0d0))
;;            (the double-float (+ f3 3.0d0))
;;            (the double-float (+ f4 4.0d0))
;;            (the double-float (+ f5 5.0d0))
;;            (the double-float (+ f6 6.0d0))
;;            (the double-float (+ f7 7.0d0))
;;            (the double-float (+ f8 8.0d0))
;;            (the double-float (+ f9 9.0d0))
;;            (the double-float (+ f10 10.0d0)))))

;; (let ((f1 0.0d0)
;;       (f2 0.0d0)
;;       (f3 0.0d0)
;;       (f4 0.0d0)
;;       (f5 0.0d0)
;;       (f6 0.0d0)
;;       (f7 0.0d0)
;;       (f8 0.0d0)
;;       (f9 0.0d0)
;;       (f10 0.0d0))
;;   (declare (type double-float f1 f2 f3 f4 f5 f6 f7 f8 f9 f10))
;;   (time (dotimes (i 100000000)
;;           (fast-progn
;;            (multiple-value-bind (p nf1 nf2) (u1 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10)
;;                                         ;(my-debug f1 f2)
;;              (setf f1 nf1
;;                    f2 nf2
;;                    f3 nf2
;;                    f4 nf2
;;                    f5 nf2
;;                    f6 nf2
;;                    f7 nf2
;;                    f8 nf2
;;                    f9 nf2
;;                    f10 nf2)
;;              p)))))
(declaim (inline lupdate))
(defun lupdate (l)
  (symbol-macrolet  ((f1 (car l))
                     (f2 (cadr l))
                     (f3 (caddr l)))
    (declare (type symbol f1)
             (type double-float f2)
             (type fixnum f3))
    (fast-progn 
     ;; (my-debug l)
     (setf f1 :above)
     (setf f2 (the double-float (+ 1.0d0 f2)))
     (setf f3 (the fixnum (+ 10 f3)))
     ;; (my-debug l)
     (values 1 l))))
(let* ((l (list :init 0.0d0 1)))
  (time (fast-progn
         (dotimes (i 100000000)
           (declare (type fixnum i))
           (multiple-value-bind (p ln) (lupdate l)
             p
             (setf l ln)))
         (my-debug l))))
