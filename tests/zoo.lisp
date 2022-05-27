(defpackage tests/zoo
  (:use :cl :ptester)
  (:import-from :zoo
	       #:fits-p
	       #:make-spider
	       #:make-elephant
	       #:make-monkey)
  (:export #:zoo-examples))

(in-package :tests/zoo)

;; =============================================================================
;;;; EXAMPLES

(defparameter *orb-weaver*
  (zoo:make-spider :space 1e-2)
  "Represents a spider and the space it takes up.")

(defparameter *dumbo*
  (zoo:make-elephant :space 150)
  "An example of an elephant, which takes up less space.")

(defparameter *dumbos-mom*
  (zoo:make-elephant)
  "An example of an elephant, which takes up all the space.")

(defparameter *caesar*
  (zoo:make-monkey :intelligence 100
		   :space 20)
  "An example of a highly intelligent monkey that doesn't take up much space.")

;; =============================================================================
;;;; FUNCTIONS

(defun zoo-examples (ok bad)
  "Examples for testing external Zoo functions."
  (terpri)
  (ptester:with-tests (:name "FITS-P: Tests if the cage is large enough")
    (test T (fits-p *caesar* 50))
    (test NIL (fits-p *caesar* 10))
    (test T (fits-p *orb-weaver* 1))
    (test NIL (fits-p *orb-weaver* 1e-5))
    (test T (fits-p *dumbo* 400))
    (test NIL (fits-p *dumbo* 100))
    (test T (fits-p *dumbos-mom* 500))
    (test NIL (fits-p *dumbos-mom* 390))

    (incf ok *test-successes*)
    (incf bad *test-errors*))
  (terpri)

  (list ok bad))
