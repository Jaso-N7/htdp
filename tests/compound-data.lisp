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
		#:make-time #:copy-time
		#:time-p #:time-hours #:time-minutes #:time-seconds


		;; Shapes
		#:distance-to-0
		#:perimeter
		#:area
		#:make-posn #:posn-x #:posn-y
		#:make-circle #:circle-pos #:circle-s
		#:make-square #:square-pos #:square-s)
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
    (terpri)

    (with-tests (:name "DISTANCE-TO-0: Can deduce the distance to origin.")
      (test 0 (distance-to-0 0) :test #'=)
      (test 10 (distance-to-0 10) :test #'=)
      (test 5.3851647 (distance-to-0 (htdc:make-posn :x 5 :y 2)) :test #'=)

      (incf ok *test-successes*)
      (incf bad *test-errors*))

    (terpri)
    (with-tests (:name "PERIMETER: Calculates a Shapes perimeter.")
      (test 16 (htdc:perimeter (htdc:make-square
				:pos (htdc:make-posn :x 10 :y 10)
				:s 4))
	    :test #'=)
      (test (* 40 pi) (htdc:perimeter (htdc:make-circle
				       :pos (htdc:make-posn :x 30 :y 30)
				       :s 20))
	    :test #'=)

      (incf ok *test-successes*)
      (incf bad *test-errors*))

    (terpri)
    (with-tests (:name "AREA: Calculates a Shapes perimeter.")
      ;; Area of a Square = side x side = side^2 square units
      (test (* 4 4)
	    (htdc:area (htdc:make-square
			:pos (htdc:make-posn :x 10 :y 10)
			:s 4))
	    :test #'=)
      ;; Area of a Circle = pi.r^2
      (test (* pi (* 20 20)) (htdc:area (htdc:make-circle
					 :pos (htdc:make-posn :x 30 :y 30)
					 :s 20))
	    :test #'=)
      (test 78.5 (htdc:area (htdc:make-circle 
			     :pos (htdc:make-posn 
				   :x 0 :y 0) 
			     :s 5)))

      (incf ok *test-successes*)
      (incf bad *test-errors*))

    (terpri))

  (list ok bad))
