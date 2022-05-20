(defpackage section-6-tests
  (:documentation "Test package for Compound Data.")
  (:use :cl :ptester)
  (:import-from :htdc
		#:within-range
		#:reduce-range
		#:make-jet-fighter
		#:jet-fighter-p
		#:jet-fighter-designation
		#:jet-fighter-acceleration
		#:jet-fighter-top-speed
		#:jet-fighter-range)
  (:export #:test-compound-data))

(in-package :section-6-tests)

;; Number Number -> (Number Number)
(defun test-compound-data (ok bad)
  "Contains all examples for testing Compound Data"
  
  (let ((jf (make-jet-fighter
	     :designation 'lavi
	     ;; Accel is inaccurate, just need something for the test.
	     :acceleration 20
	     :top-speed 1221
	     :range 2300)))

    (with-tests (:name "Testing JET-FIGHTER")
      (test T (jet-fighter-p jf))
      (test 'LAVI (jet-fighter-designation jf))
      (test 20 (jet-fighter-acceleration jf) :test #'=)
      (test 1221 (jet-fighter-top-speed jf) :test #'=)
      (test 2300 (jet-fighter-range jf) :test #'=)

      (incf ok *test-successes*)
      (incf bad *test-errors*))
    (terpri)

    (with-tests (:name "Is target within range of (fighter's) base?")
      (test T (within-range jf 800))
      (test NIL (within-range jf 3000))

      (incf ok *test-successes*)
      (incf bad *test-errors*))
    (terpri)

    (with-tests (:name "Can reduce Jet Fighter's range.")
      (test (* (jet-fighter-range jf) .8) (jet-fighter-range (reduce-range jf))
	    :test #'=)
      (test 480 (jet-fighter-range
		 (reduce-range (make-jet-fighter :range 600)))
	    :test #'=)))


  (list ok bad))
