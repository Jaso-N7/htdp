(defpackage section-6-tests
  (:documentation "Test package for Compound Data.")
  (:use :cl :ptester)
  (:import-from :htdc
		;; JetFighter
		#:within-range
		#:reduce-range
		#:make-jet-fighter
		#:jet-fighter-p
		#:jet-fighter-designation
		#:jet-fighter-acceleration
		#:jet-fighter-top-speed
		#:jet-fighter-range

		;; Time
		#:time->seconds
		#:make-time
		#:copy-time
		#:time-p
		#:time-hours
		#:time-minutes
		#:time-seconds)
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
	    :test #'=)

      (incf ok *test-successes*)
      (incf bad *test-errors*))
    (terpri)

    (with-tests (:name "TIME->SECONDS: Can reduce time to seconds.")
      (test 45002 (htdc:time->seconds (make-time
				       :hours 12
				       :minutes 30
				       :seconds 2)))
      (test 5400 (htdc:time->seconds (make-time
				      :hours 1
				      :minutes 30)))
      (test 3782 (htdc:time->seconds (make-time
				      :hours 1
				      :minutes 3
				      :seconds 2)))
      (test 3600 (htdc:time->seconds (make-time
				      :hours 1)))
      (test 60 (htdc:time->seconds (make-time
				    :minutes 1)))
      (test 1 (htdc:time->seconds (make-time
				   :seconds 1)))
      ;; No one can make time
      (test 0 (htdc:time->seconds (make-time)))

      (incf ok *test-successes*)
      (incf bad *test-errors*))
    (terpri))

  (list ok bad))
