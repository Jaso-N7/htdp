(defpackage section-7-tests
  (:use :cl :ptester)
  (:import-from :mixed
	       #:fits-p
	       #:make-spider
	       #:make-elephant
	       #:make-monkey)
  (:export #:zoo-examples))

(in-package :section-7-tests)

;; =============================================================================
;;;; EXAMPLES

;; ======= Zoo Animals

(defparameter *orb-weaver*
  (make-spider :space 1e-2)
  "Represents a spider and the space it takes up.")

(defparameter *dumbo*
  (make-elephant :space 150)
  "An example of an elephant, which takes up less space.")

(defparameter *dumbos-mom*
  (make-elephant)
  "An example of an elephant, which takes up all the space.")

(defparameter *caesar*
  (make-monkey :intelligence 100
		   :space 20)
  "An example of a highly intelligent monkey that doesn't take up much space.")

;; ======= Vehicles

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
