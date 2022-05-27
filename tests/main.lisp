(defpackage htdp/tests
  (:use :cl)
  (:import-from :htdf-tests
		#:test-functions)
  (:import-from :htdd-tests
		#:test-examples)
  (:import-from :section-5-tests
		#:test-section-5)
  (:import-from :section-6-tests
		#:test-compound-data)
  (:import-from :section-7-tests
		#:zoo-examples)
  (:export :resolve))

(in-package :htdp/tests)

;; NOTE: To run this test file, execute `(asdf:test-system :htdp)' in your Lisp.

(let ((ptester:*test-successes* 0)
      (ptester:*test-errors* 0))
  (defun probe (&key (ok ptester:*test-successes*)
		  (bad ptester:*test-errors*))
    "Test all examples."

    (let ((accum (mapcar #'+
			 (test-functions ok bad)
			 (test-examples ok bad)
			 (test-section-5 ok bad)
			 (test-compound-data ok bad)
			 (zoo-examples ok bad))))

      (princ "Examples revealed the following:")
      (format t "~&|  Total Successes: ~A~%|  Total Errors Detected: ~A~%"
	      (first accum)
	      (second accum))
      ;; !!! TODO: Only run Property-Based Testing once (= 0 (second accum))
      )))
