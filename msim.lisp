(in-package :msim)

(defun test (a)
  (format t "~a" a))

(test "a")


(defun simulate-strat (strat pcs)
  (declare (type (simple-array double-float (*)) pcs)
           (type function strat))
  #f
  (let* ((n (length pcs))
         (pos (make-array n :element-type 'fixnum)))
    (declare (type fixnum n))
    (dotimes (i n)
      (declare (type fixnum i))
      (setf (aref pos i) (the fixnum (funcall strat (aref pcs i)))))))

(defun compile-strat (strat)
  )


(defun bootstrap-sum (arr inds)
  (let ((res 0))
    (loop for elem in inds do
         (incf res)))
  )


(defun bootstrap-sum (a inds)
  #f
  (declare (type (simple-array fixnum (*)) a)
           (type (simple-array fixnum (*)) inds))
  (let ((n (length inds))
        (result 0))
    (declare (fixnum n)
             (fixnum result))
    (dotimes (i n)
      (incf result (aref a i)))
    result))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun strategy-function (strat)
    (cond ((atom strat) strat)
          (t (apply (car strat) (mapcar #'strategy-function (cdr strat)))))))

(defmacro compile-strategy (strat n)
  (let* ((strat (strategy-function strat)))
    `(multiple-value-bind ,(synth :state strat 'strategy) (values ,@(synth :init strat)) 
       (dotimes (i ,n)
         (let* ((price (sin (* (/ (* 2.0 pi 2.0) ,(- n 1)) i))))
           (declare (type double-float price)) 
           ;; (multiple-value-bind ,(synth :state strat 'strategy) ,(synth :update strat 'strategy))
           ,(synth :update strat 'strategy)
           ;; (multiple-value-bind (state) ,(synth :update strat 'state))
           ;; (format t "i=~a price=~a ema=~a p=~a~%" i ,@(synth-all :name indicators) p)
           )))))

(defprim ema (alpha)
  (:pretty () (list 'ema (list :alpha alpha)))
  (:init () (list 0.0d0))
  (:state (name) (list (symb name "-STATE")))
  (:update (name) `(let ((v (+ (* ,alpha price) (* ,(- 1.0d0 alpha) ,(symb name "-STATE")))))
                     (values v v))))

;; (defmacro ema (alpha)
;;   `(ema% (gensim "ema" ,alpha)))

(defprim price () 
  (:pretty () (list 'price))
  (:init () nil)
  (:state (*) nil)
  (:update (*) `(values price)))


(defmacro defbasic (name pos) 
  `(defprim ,name () 
     (:pretty () (list ',name))
     (:init () nil)
     (:state (*) nil)
     (:update (*) nil)
     (:position (*) ,pos)))
(defbasic bullish 1)
(defbasic bearish -1)
(defbasic neutral 0)

;; (defmacro tvalues (&rest args)
;;   `(values ,@(remove nil args)))

(defprim cross (fast slow bullish bearish)
  (:pretty () (list 'cross (list :fast (synth :pretty fast) :slow (synth :pretty slow)
                                 :bullish (synth :pretty bullish) :bearish (synth :pretty bearish))))
  (:init () `(0 ,@(synth :init fast) ,@(synth :init slow) ,@(synth :init bullish) ,@(synth :init bearish))) 
  (:state (name) (let ((fast-name (symb name "-FAST"))
                       (slow-name (symb name "-SLOW"))
                       (bullish-name (symb name "-BULLISH"))
                       (bearish-name (symb name "-BEARISH"))) 
                   `(,(symb name "-POSITION") ,@(synth :state fast fast-name) ,@(synth :state slow slow-name)
                      ,@(synth :state bullish bullish-name) ,@(synth :state bearish bearish-name))))
  (:update (name) 
           (let* ((position-name (symb name "-POSITION"))
                  (fast-name (symb name "-FAST"))
                  (slow-name (symb name "-SLOW"))
                  (bullish-name (symb name "-BULLISH"))
                  (bearish-name (symb name "-BEARISH"))
                  (fast-state (synth :state fast fast-name))
                  (fast-value (symb name "-FAST-VALUE"))
                  (slow-value (symb name "-SLOW-VALUE"))
                  (slow-state (synth :state slow slow-name))
                  (bullish-state (synth :state bullish bullish-name))
                  (bearish-state (synth :state bearish bearish-name)))
             `(with-multiple-value-bindings (((,fast-value ,@fast-state) ,(synth :update fast fast-name))
                                             ((,slow-value ,@slow-state) ,(synth :update slow slow-name))
                                             (,bullish-state ,(synth :update bullish bullish-name))
                                             (,bearish-state ,(synth :update bearish bearish-name)))
                (let ((,position-name (if (>= ,fast-value ,slow-value) 
                                          ,(synth :position bullish bullish-name)
                                          ,(synth :position bearish bearish-name)))) 
                  ;; (pprint (list ,position-name ,@fast-state ,@slow-state 
                  ;;               ,@bullish-state ,@bearish-state))
                  (values ,position-name ,@fast-state ,@slow-state 
                          ,@bullish-state ,@bearish-state)))))
  (:position (name) (symb name "-POSITION")))


 
;; (defprim cross-from-below (s1 s2)
;;   (:pretty () (list 'cross-from-below (list :s1 (synth :pretty s1) :s2 (synth :pretty s2))))
;;   (:init () `(,@(synth :init s1) ,@(synth :init s2)))
;;   (:state (name) (let ((s1-name (symb name "-S1"))
;;                        (s2-name (symb name "-S2"))) 
;;                    `(,@(synth :state s1 s1-name) ,@(synth :state s2 s2-name))))
;;   (:condition (name) `(>= ,(symb name "-S1") ,(symb name "-S2"))))

;; (defprim cross-from-above (s1 s2)
;;   (:pretty () (list 'cross-from-above (list :s1 (synth :pretty s1) :s2 (synth :pretty s2))))
;;   (:init () `(,@(synth :init s1) ,@(synth :init s2)))
;;   (:state (name) (let ((s1-name (symb name "-S1"))
;;                        (s2-name (symb name "-S2"))) 
;;                    `(,@(synth :state s1 s1-name) ,@(synth :state s2 s2-name))))
;;   (:condition (name) `(<= ,(symb name "-S1") ,(symb name "-S2"))))

;; (defprim wait-as (before event after)
;;   (:pretty () (list 'before (list :before (synth :pretty before)
;;                                   :event (synth :pretty bullish) :bearish (synth :pretty bearish))))
;;   (:init () `(0 ,@(synth :init fast) ,@(synth :init slow) ,@(synth :init bullish) ,@(synth :init bearish))) 
;;   (:state (name) (let ((before-name (symb name "-BEFORE"))
;;                        (after-name (symb name "-AFTER"))) 
;;                    `(,(symb name "-POSITION") ,@(synth :state before before-name) ,@(synth :state after after-name))))
;;   (:update (name) 
;;            (let* ((position-name (symb name "-POSITION"))
;;                   (before-name (symb name "-BEFORE"))
;;                   (after-name (symb name "-AFTER"))
;;                   (event-name (symb name "-EVENT")) 
;;                   (event-state (synth :state event event-name))
;;                   (before-state (synth :state before before-name))
;;                   (before-value (symb name "-BEFORE-VALUE"))
;;                   (after-value (symb name "-AFTER-VALUE"))
;;                   (after-state (synth :state after after-name)))
;;       (prog1 (funcall state x)
;;         (if (funcall event x)
;;             (setf state (force after))))

;;              `(if (synth :condition event event-name) 
;;                   (with-multiple-value-bindings (((,before-value ,@before-state) ,(synth :update before before-name))
;;                                                  ((,after-value ,@after-state) ,(synth :update after after-name))
;;                                                  (,bullish-state ,(synth :update bullish bullish-name))
;;                                                  (,bearish-state ,(synth :update bearish bearish-name)))
;;                     (let ((,position-name (if (>= ,before-value ,after-value) 
;;                                               ,(synth :position bullish bullish-name)
;;                                               ,(synth :position bearish bearish-name)))) 
;;                       ;; (pprint (list ,position-name ,@before-state ,@after-state 
;;                       ;;               ,@bullish-state ,@bearish-state))
;;                       (values ,position-name ,@before-state ,@after-state 
;;                               ,@bullish-state ,@bearish-state))))))
;;   (:position (name) (symb name "-POSITION")))



(defun main3 ()
  ;; (compile-strategy (cross (price) (ema 0.1d0) 
  ;;                          (cross (price) (ema 0.8d0) (bullish) (neutral))
  ;;                          (cross (price) (ema 0.8d0) (neutral) (bearish))) 1000000001)
  
 (compile-strategy (cross (price) (ema 0.1d0) 
                           (bullish)
                           (bearish)) 1000000001)
  ;; (pprint (synth :indicators strat))
  )

(time (main3))



