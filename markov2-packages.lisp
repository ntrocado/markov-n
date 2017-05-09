(in-package "COMMON-LISP-USER")

(ql:quickload :alexandria)

(defpackage :alias-method
  (:use :common-lisp)
  (:export :make-discrete-random-var))

(defpackage :markov2
  (:use :common-lisp :alexandria :alias-method))

(defpackage :markov-n
  (:use :common-lisp :alexandria :alias-method))
