(defpackage htdp
  (:documentation "A Systematic Designs of Programs. Learning How to Design Programs.")
  (:use :cl :htdf :htdd)
  (:export #:check-guess
	   #:guess-with-tui
	   #:reveal-target))
(in-package :htdp)

(defun main ()
  "Random entry point for the application"
  (format t "~&Want to try the guessing game?
> (guess-with-tui #'check-guess)"))
