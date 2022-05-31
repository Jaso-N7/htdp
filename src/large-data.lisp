;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: large-data -*-
;;;
;;; How to design for Arbitrarily Large Data
;;; Ref: HtDP/2e Section II Arbitrarily Large Data
;;;
;;; Author: Jason Robinson
;;; Created: Tue 05/31/2022
;;;
;;; ============================================================================

(defpackage large-data
  (:documentation "Practise with designing programs of arbitrarily large data.")
  (:use :cl :ptester))

(in-package :large-data)

;;; ============================================================================
;;;; DATA DEFINITIONS

;; A (Names) is one of:
;; - '()
;; - (cons String (Names))
;; INTERPRETATION: a list of invitees, by last name

(defparameter *lon1* '()
  "An example of an empty (Names)")
(defparameter *lon2*
  (cons "Krishnamurthi"
	(cons "Felleisen"
	      (cons "Flatt"
		    (cons "Findler"
			  (cons "Stuttgard" '())))))
  "Exercise 130: A (Names) that contains five Names")

;; Recursive Template
#+(or)
;; f-for-names : (Names) -> ???
(defun f-for-names (names)
  (cond ((endp names) nil)
	((eq (first names)
	     nil)
	 T)
	(t (f-for-names (rest names)))))

;; A (Booleans) is one of:
;; - '()
;; - (cons Boolean (Booleans))
;; INTERPRETATION: a list of Booleans. Take note that '() is also NIL

(defparameter *lob1*
  (cons t (cons nil (cons t '())))
  "Exercise 131: An example of a list of booleans (Booleans).")

;;; ============================================================================
;;;; FUNCTIONS

;; See Chapter 8.3
#| SAMPLE PROBLEM
You are working on the contact list for some new cell phone. The phone’s owner
updates and consults this list on various occasions. For now, you are assigned
the task of designing a function that consumes this list of contacts and
determines whether it contains the name "Flatt".
|#

;; (Names) -> Boolean
;; Determines whether "Flatt" is in a list of names
;; (defun contains-flatt-p (names) (declare (ignore names)) nil) ; stub

;; EXAMPLES
(with-tests (:name "Does the list contain a specific name?")
  (ptester:test NIL (contains-flatt-p '()))
  (ptester:test NIL (contains-flatt-p (cons "Find" '())))
  (ptester:test T (contains-flatt-p (cons "Flatt" '())))
  (ptester:test T (contains-flatt-p (cons "A" (cons "Flatt" (cons "C" '())))))
  ;; Made a general example for which the answer must be false
  (ptester:test NIL (contains-flatt-p (cons "A" (cons "B" (cons "C" '()))))))

;; Template
#+(or)
(defun contains-flatt-p (names)
  (cond ((endp names) nil)
	((consp names) (first names))
	(t (rest names))))

(defun contains-flatt-p (names)
  "Searches a list of names for friend called Flatt"
  (cond ((endp names) nil)
	((string= (first names) "Flatt") T)
	(t  (contains-flatt-p (rest names)))))

;; Ex 134
;; contains-p : String (Names) -> Boolean
;; Determines whether some given string occurs on a given list of strings.
;; (defun contains-p (name names) (declare (ignore name names)) nil) ; stub

(with-tests (:name "CONTAINS-P")
  (ptester:test NIL (contains-p "Flatt" '()))
  (ptester:test NIL (contains-p "Flatt" (cons "Find" '())))
  (ptester:test T (contains-p "Flatt" (cons "Flatt" '())))
  (ptester:test T (contains-p "Flatt" (cons "A" (cons "Flatt" (cons "C" '())))))
  ;; Made a general example for which the answer must be false
  (ptester:test NIL (contains-p "Flatt" (cons "A" (cons "B" (cons "C" '()))))))

;; <template used (Names)>
(defun contains-p (name names)
  "Determines whether some given string occurs on a given list of strings.
Similar to a call to MEMBER, however it returns a BOOLEAN instead of a list."
  ;; (declare (notinline contains-p)) ; Tracing purposes
  (cond ((endp names) NIL)
	((string= name (first names)) T)
	(t (contains-p name (rest names)))))
