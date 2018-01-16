(in-package :msim)

(defmacro multiple-value-do (bindings (end-test &rest results) &body body)
  `(block nil
     (with-multiple-value-bindings ,(mapcar (lambda (binding) (list (car binding) (cadr binding))) 
                                            bindings)
       (loop (when ,end-test (return (progn ,@results)))
          ,@body
          (with-multiple-value-bindings ,(mapcar (lambda (binding) (list (mapcar (lambda (var) (symb 'new- var))
                                                                                 (car binding))
                                                                         (caddr binding))) 
                                                 bindings)
            (progn ,@(apply #'append (mapcar (lambda (binding) 
                                         (mapcar (lambda (var step)
                                                   `(setq ,var ,step))
                                                 (car binding)
                                                 (mapcar (lambda (var) (symb 'new- var))
                                                         (car binding)))) 
                                       bindings)


                      ;; (loop for binding in bindings collecting (mapcar (lambda (var step)
                      ;;                                                    `(setq ,var ,step))
                      ;;                                                  (list (mapcar (lambda (var) (symb 'new- var))
                      ;;                                                                (car binding))
                      ;;                                                        (caddr binding))
                      ;;                                                  (caddr binding)))
                      )))))))

;; (let ((n 100000000)) 
;;   (time (multiple-value-do (((a b) (values 0 0) (values (1+ a) (1+ b)))) 
;;             ((equal a n) 'done)
;;      ;; (format t "hello ~a ~a~%" a b)
;;      )))
