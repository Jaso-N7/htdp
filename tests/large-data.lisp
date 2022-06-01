(defpackage large-data-tests
  (:documentation "Tests for Arbitrarily Large Data")
  (:use :cl :ptester)
  (:import-from :large-data
		#:contains-flatt-p
		#:contains-p)
  (:export #:large-data-examples))

(in-package :large-data-tests)

(defun large-data-examples (ok bad)

  (with-tests (:name "CONTAINS-FLATT-P: Does the list contain a 'Flatt'?")
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

  (list ok bad))
