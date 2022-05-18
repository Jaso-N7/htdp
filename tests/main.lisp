(defpackage htdp/tests
  (:use :cl)
  (:import-from
   :htdf-tests
   :test-functions)
  (:import-from
   :htdd-tests
   :test-examples)
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
			 (test-examples ok bad))))

      (princ "Examples revealed the following:")
      (format t "~&Total Successes: ~A~%Total Errors Detected: ~A~%"
	      (first accum)
	      (second accum)))))
