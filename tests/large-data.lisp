(defpackage large-data-tests
  (:documentation "Tests for Arbitrarily Large Data")
  (:use :cl :ptester)
  (:import-from :large-data
		#:contains-flatt-p
		#:contains-p
		#:sum
		#:pos-p
		#:checked-sum
		#:count-str
		#:wage*
		#:convert-fc)
  (:export #:large-data-examples))

(in-package :large-data-tests)

(defun large-data-examples (ok bad)

  (with-tests (:name "CONTAINS-FLATT-P: Does the list contain the name 'Flatt'?")
    (ptester:test NIL (contains-flatt-p '()))
    (ptester:test NIL (contains-flatt-p (cons "Find" '())))
    (ptester:test T (contains-flatt-p (cons "Flatt" '())))
    (ptester:test T (contains-flatt-p (cons "A" (cons "Flatt" (cons "C" '())))))
    ;; Made a general example for which the answer must be false
    (ptester:test NIL (contains-flatt-p (cons "A" (cons "B" (cons "C" '())))))

    (incf ok ptester:*test-successes*)
    (incf bad ptester:*test-errors*))

  (terpri)
  
  (with-tests (:name "CONTAINS-P: Does the list contain a specified name?")
    (ptester:test NIL (contains-p "Flatt" '()))
    (ptester:test NIL (contains-p "Flatt" (cons "Find" '())))
    (ptester:test T (contains-p "Flatt" (cons "Flatt" '())))
    (ptester:test T (contains-p "Flatt" (cons "A" (cons "Flatt" (cons "C" '())))))
    ;; Made a general example for which the answer must be false
    (ptester:test NIL (contains-p "Flatt" (cons "A" (cons "B" (cons "C" '())))))

    (incf ok ptester:*test-successes*)
    (incf bad ptester:*test-errors*))

  (terpri)
  (with-tests (:name "SUM: Computes the sum of the amounts")
    (test 0 (sum large-data::*loa1*) :test #'=)
    (test 7.0 (sum large-data::*loa2*) :test #'=)
    (test 752.25 (sum large-data::*loa3*) :test #'=)

    (incf ok ptester:*test-successes*)
    (incf bad ptester:*test-errors*))

  (terpri)
  (with-tests (:name "POS-P: Are all numbers positive?")
    (test NIL (pos-p large-data::*lon1*))
    (test T (pos-p large-data::*lon2*))
    (test NIL (pos-p large-data::*lon3*))
    (test T (pos-p (cons 1 (cons 2.3 (cons pi (cons 5.678 '())))))))

  (terpri)
  (with-tests (:name "CHECKED-SUM: Produces a sum or an error")
    (test 10 (checked-sum (cons 7 (cons 3 '()))))
    (test-error (checked-sum (cons -7 (cons 3 '())))))

  (terpri)
  (ptester:with-tests (:name "COUNT-STR")
    (ptester:test 0 (count-str '("this" "is" "a" "list") "string"))
    (ptester:test 1 (count-str '("this" "is" "a" "list") "list"))
    (ptester:test 2 (count-str '("this" "is" "a" "list" "of" "list") "list")))

  (terpri)
  (with-tests (:name "WAGE*")
    (test '() (wage* '()))
    (test '(336) (wage* '(28) 12) :test #'equal)
    (test '(576 288) (wage* '(48 24) 12) :test #'equal)
    (test '(0 0) (wage* '(4 2)) :test #'equal)
    (test '() (wage* '() 14))
    (test '(392) (wage* '(28) 14) :test #'equal)
    (test '(672 336) (wage* '(48 24) 14) :test #'equal)
    (test-error (wage* '(101)))
    (test-error (wage* '(24 40 60 100 120)))

    (incf ok *test-successes*)
    (incf bad *test-errors*))
  (terpri)

  (with-tests (:name "CONVERT-FC")
    (test '() (convert-fc '()))
    (test-error (convert-fc '(32 -500 -21 56.7)))
    (test '(-273.15002 -40.0 -17.777779 0.0 37.0 100.0)
	  (convert-fc '(-459.67 -40 0 32 98.6 212))
	  :test #'equal))

  (terpri)
  (list ok bad))
