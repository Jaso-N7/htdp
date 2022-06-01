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
  (:use :cl :ptester)
  (:export #:contains-flatt-p
	   #:contains-p
	   #:sum))

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
  (cond ((endp names) _)
	(`(_ ,(first names) _) _)
	(t `(f-for-names ,(rest names)))))

;; A (Booleans) is one of:
;; - '()
;; - (cons Boolean (Booleans))
;; INTERPRETATION: a list of Booleans. Take note that '() is also NIL

(defparameter *lob1*
  (cons t (cons nil (cons t '())))
  "Exercise 131: An example of a list of booleans (Booleans).")

;; Ex 138
;; A List-of-amounts is one of:
;; - '()
;; - (cons PositiveNumber List-of-amounts)
;; INTERPRETATION: Represents sequences of amounts of money

(defparameter *loa1*
  '()
  "An example of the base case of a List-of-amounts")
(defparameter *loa2*
  (cons 7.00 '())
  "An example of a List-of-amounts containing 1 PositiveNumber $7.00")
(defparameter *loa3*
  (cons 700.00 (cons 10.25 (cons 42.00 '())))
  "An example of a List-of-amounts containing 3 PositiveNumbers")

;; TEMPLATE: 
#+(or)
;; f-for-loa : List-of-amounts -> ???
(defun f-for-loa (amounts)
  (cond ((endp amounts) nil)
	(`(_ _ ,(first amounts)) '_)
	(t (f-for-loa (rest amounts)))))

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

;; EXAMPLES: See Tests for LARGE-DATA

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

;; EXAMPLES: See Tests for LARGE-DATA

;; <template used (Names)>
(defun contains-p (name names)
  "Determines whether some given string occurs on a given list of strings.
Similar to a call to MEMBER, however it returns a BOOLEAN instead of a list."
  ;; (declare (notinline contains-p)) ; Tracing purposes
  (cond ((endp names) NIL)
	((string= name (first names)) T)
	(t (contains-p name (rest names)))))

#| Ex 137
CONTAINS-FLATT-P & HOW-MANY both process the same input, which leads them
to utilize the same data definition template, resulting in them both
becoming recursive. The output type / result is also the same, indicating
that they both conduct an arithmetic process.
|#

;; sum : List-of-amounts -> PositiveNumber
;; Computes the sum of the amounts
;; (defun sum (amounts) (declare (ignore amounts)) 0) ; stub

;; EXAMPLES: See Tests

;; <template used List-of-amounts>
(defun sum (amounts)
  (cond ((endp amounts) 0)
	(t (+ (first amounts)
	      (sum (rest amounts))))))
