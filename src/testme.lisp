(defpackage testme 
  (:use :common-lisp)
  (:export hello-world))
(defun testme:hello-world () (print "hello"))
(testme:hello-world)