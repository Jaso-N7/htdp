(defpackage section-5-tests
  (:documentation "Tests for HTDP Section 5")
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
  (:import-from :htdp
		#:check-guess)
  (:export #:test-section-5))

(in-package :section-5-tests)

;; Number Number -> (Number Number)
(defun test-section-5 (ok bad)
  "Confirm all exercises from HTDP 1e section 5"
  (ptester:with-tests (:name "CHECK-GUESS: Can you guess the correct number.")
    (test htdp:*small* (check-guess 1 10))
    (test htdp:*perfect* (check-guess 42 42))
    (test htdp:*high* (check-guess 1000 1))

    (incf ok *test-successes*)
    (incf bad *test-errors*))

  (terpri)

  (list ok bad))
