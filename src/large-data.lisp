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
(defun contains-flatt-p (names) nil)

;; EXAMPLES
(with-tests (:name "Does the list contain a specific name?")
  (ptester:test NIL (contains-flatt-p '()))
  (ptester:test NIL (contains-flatt-p (cons "Find" '())))
  (ptester:test T (contains-flatt-p (cons "Flatt" '())))
  (ptester:test T (contains-flatt-p (cons "A" (cons "Flatt" (cons "C" '())))))
  ;; Made a general example for which the answer must be false
  (ptester:test NIL (contains-flatt-p (cons "A" (cons "B" (cons "C" '()))))))
