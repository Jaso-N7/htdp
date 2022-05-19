(defpackage htdd-tests
  (:documentation "Testing for the HTDD solutions.")
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
		#:*TEST-SUCCESSES* )
  (:import-from :htdd
		#:bump-up
		#:middle-seat-p
		#:aisle-seat-p
		#:traffic-light-next)
  (:export :test-examples))

(in-package :htdd-tests)

;; =====================================================================
;;;; TESTS --- Examples

;; Number Number -> (Number Number)
(defun test-examples (ok bad)
  "Ensure all functions return as expected"
  (with-tests (:name "TRAFFIC-LIGHT-NEXT Yields the next state given
 the current state")
    (test "red" (traffic-light-next "yellow") :test #'string=)
    (test "yellow" (traffic-light-next "green") :test #'string=)
    (test "green" (traffic-light-next "red") :test #'string=)

    (incf ok *test-successes*)
    (incf bad *test-errors*))
  (terpri)
  (with-tests (:name "AISLE-SEAT-P checks if a seat number is an aisle seat")
    (test T (aisle-seat-p 1))
    (test NIL (aisle-seat-p 12))
    (test T (aisle-seat-p 32))

    (incf ok *test-successes*)
    (incf bad *test-errors*))
  (terpri)
  (with-tests (:name "MIDDLE-SEAT-P checks if a seat number is an aisle seat")
    (test NIL (middle-seat-p 1))
    (test T (middle-seat-p 16))
    (test NIL (middle-seat-p 32))

    (incf ok *test-successes*)
    (incf bad *test-errors*))
  (terpri)
  (with-tests (:name "BUMP-UP produces next higher grade")
    (test "A" (bump-up "A") :test #'string=)
    (test "A" (bump-up "B") :test #'string=)
    (test "B" (bump-up "C") :test #'string=)

    (incf ok *test-successes*)
    (incf bad *test-errors*))
  (terpri)

  (list ok bad))

;; =====================================================================
;;;; TESTS --- Properties
