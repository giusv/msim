(in-package :msim)

(defmacro! multiple-value-do (bindings (end-test &rest results) &body body)
  `(block nil
     (with-multiple-value-bindings ,(mapcar (lambda (binding) (list (car binding) (cadr binding))) 
                                            bindings)
       (loop (when ,end-test (return (progn ,@results)))
          ,@body
          (with-multiple-value-bindings ,(mapcar (lambda (binding) (list (car binding) (cadr binding))) 
                                                 bindings)
           ,@(apply #'append (loop for binding in bindings collecting (mapcar (lambda (var step)
                                                                                `(setq ,var ,step))
                                                                              (car binding)
                                                                              (caddr binding)))))))))

(multiple-value-do (((a b) (values 0 0) ((1+ a) (1+ b)))) 
    ((equal a 10) 'done)
  (format t "hello"))
