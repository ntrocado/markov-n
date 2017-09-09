(in-package "COMMON-LISP-USER")

(defpackage :alias-method
  (:use :common-lisp)
  (:export :make-discrete-random-var))

(defpackage :markov-n
  (:use :common-lisp :alexandria :alias-method))
