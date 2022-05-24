(defpackage tests/zoo
  (:use :cl :ptester)
  (:import-from :zoo
	       #:fits-p
	       #:make-spider
	       #:make-elephant
	       #:make-monkey)
  (:export #:zoo-examples))

(in-package :tests/zoo)

(defun zoo-examples (ok bad)
  "Examples for testing external Zoo functions."
  (terpri)
  (ptester:with-tests (:name "FITS-P: Tests if the cage is large enough")
    (test 1 1)

    (incf ok *test-successes*)
    (incf bad *test-errors*))
  (terpri)

  (list ok bad))
