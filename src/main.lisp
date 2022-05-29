(defpackage htdp
  (:documentation "A Systematic Designs of Programs. Learning How to Design Programs.")
  (:use :cl :htdf :htdd :htdc)
  (:export #:check-guess
	   #:guess-with-tui
	   #:reveal-target
	   #:reply))
(in-package :htdp)

(defun main ()
  "Random entry point for the application"
  (format t "~&Want to try the guessing game?
> (guess-with-tui #'check-guess)"))

;; Ex 7.5.1
;; reply : Symbol -> Symbol
;; to determine a reply for the greeting S
(defun reply (s)
  "Determines a reply for the given symbol S"
  (check-type s symbol)
  (cond ((eq s 'GoodMorning) 'Hi)
	((eq s 'HowAreYou?) 'Fine)
	((eq s 'GoodAfternoon) (values 'I 'Need 'A 'Nap))
	((eq s 'GoodEvening) (values 'Boy 'Am 'I 'Tired))
	(t (values 'Manners 'n 'Respect))))
