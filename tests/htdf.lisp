(defpackage :htdf-tests
  (:documentation "All tests relating to the HTDF package")
  (:use :cl)
  (:import-from :ptester
		#:*BREAK-ON-TEST-FAILURES*
		#:TEST-WARNING 
		#:*ERROR-PROTECT-TESTS* 
		#:*TEST-ERRORS* 
		#:WITH-TESTS 
		#:TEST-NO-ERROR 
		#:TEST-NO-WARNING 
		#:TEST-ERROR 
		#:*TEST-UNEXPECTED-FAILURES* 
		#:TEST 
		#:*TEST-SUCCESSES*)
  (:import-from :htdf
		#:double
		#:pluralize
		#:string-first
		#:string-last
		#:string-rest
		#:string-remove-last
		#:profit
		#:revenue
		#:cost
		#:wage
		#:wage*
		#:attendees
		#:is-between-5-6-p)
  (:export #:test-functions))

(in-package :htdf-tests)

;; =====================================================================
;;;; TESTS --- Examples

;; Number Number -> (Number Number)
(defun test-functions (ok bad)
  (with-tests (:name "Produces 2 times the given number")
    (test (* 2 3) (double 3))
    (test 8.4 (double 4.2))
    (test 0 (double 0))

    (incf ok *test-successes*)
    (incf bad *test-errors*))
  (terpri)
  (with-tests (:name "Pluralizes a string")
    (test "dogs" (pluralize "dog") :test #'string=)
    (test "grasss" (pluralize "grass") :test #'string=)

    (incf ok *test-successes*)
    (incf bad *test-errors*))
  (terpri)
  (with-tests (:name "Extracts the first character from a non-empty string")
    (test #\c (string-first "character"))
    (test #\S (string-first "String"))

    (incf ok *test-successes*)
    (incf bad *test-errors*))
  (terpri)
  (with-tests (:name "Extracts the last character from a non-empty string")
    (test #\r (string-last "character"))
    (test #\g (string-last "String"))

    (incf ok *test-successes*)
    (incf bad *test-errors*))
  (terpri)
  (with-tests (:name "Returns the tail of the non-empty string.")
    (test "haracter" (string-rest "character") :test #'string=)
    (test "tring" (string-rest "String") :test #'string=)

    (incf ok *test-successes*)
    (incf bad *test-errors*))
  (terpri)
  (with-tests (:name "Returns the tail of the non-empty string.")
    (test "Cat" (string-remove-last "Cats") :test #'string=)
    (test "Dog" (string-remove-last "Dogs") :test #'string=)

    (incf ok *test-successes*)
    (incf bad *test-errors*))
  (terpri)
  (with-tests (:name "Profit: Difference between revenue and costs")
    (test 415.2 (profit 5))
    (test 889.2 (profit 4))
    (test 1063.2 (profit 3))
    (test-error (profit -5))
    (test-error (profit 0))
    (test-error (profit 'big-bucks))

    (incf ok *test-successes*)
    (incf bad *test-errors*))
  (terpri)
  (with-tests (:name "Revenue: Product of ticket price and number of attendees")
    (test (* 5.0 120) (revenue 5))
    (test 1080.0 (revenue 4))
    (test 1260.0 (revenue 3))

    (incf ok *test-successes*)
    (incf bad *test-errors*))
  (with-tests (:name "Attendees: Determine attendees based on ticket prices")
    (test 120 (attendees 5) :test #'=)
    (test 135 (attendees 4.9) :test #'=)
    (test 270 (attendees 4) :test #'=)
    (test 420 (attendees 3) :test #'=)

    (incf ok *test-successes*)
    (incf bad *test-errors*))
  (terpri)
  (with-tests (:name "Cost: Compute costs at specific ticket prices.")
    (test (+ 180 (* .04 120)) (cost 5))
    (test 190.80 (cost 4))
    (test 196.80 (cost 3))

    (incf ok *test-successes*)
    (incf bad *test-errors*))
  (terpri)
  (with-tests (:name "Wage: Calculates paycheque.")
    (test 0 (wage 12))
    (test 240 (wage 20))
    (test (* 12 30) (wage 30))
    (test 780 (wage 65))
    (test (* 40 20) (wage 20 40))
    (test 0 (wage 70))

    (incf ok *test-successes*)
    (incf bad *test-errors*))

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

  (with-tests (:name "IS-BETWEEN-5-6-P")
    (ptester:test nil (is-between-5-6-p 5))
    (ptester:test t (is-between-5-6-p 5.25))
    (ptester:test nil (is-between-5-6-p 6))
    (ptester:test-error (is-between-5-6-p 'a-number)
			:condition-type 'type-error
			:include-subtypes t)
    (incf ok *test-successes*)
    (incf bad *test-errors*))

  (terpri)

  (list ok bad))

;; =====================================================================
;;;; TESTS --- Properties
