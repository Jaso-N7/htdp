;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: htdd -*-
;;;
;;; How to Design Data
;;; =====================================================================

(defpackage htdd
  (:documentation "How to Design Data recipe.")
  (:use :cl :ptester)
  (:export #:bump-up
	   #:middle-seat-p
	   #:aisle-seat-p
	   #:traffic-light-next))

(in-package :htdd)

;;; =====================================================================
;;;; DATA DEFINITIONS

;; A TrafficLight is one of the following Strings:
;; - "red"
;; - "green"
;; - "yellow"
;; interpretation: the three strings represent the three possible states
;; that a traffic light may assume

#| PROBLEM:
Using the SeatNum data definition given below, design a function that
produces T if the given seat number is on the aisle.
|#
;; SeatNum is Natural [1,32]
;; interpretation: Seat numbers in a row, 1 and 32 are aisle seats
(defparameter sn1 1 "Aisle seat")
(defparameter sn2 12 "Middle seat")
(defparameter sn3 32 "Aisle seat")

#| Template: SeatNum

(defun fn-for-seatnum (seatnum) seatnum)

Rules used:
- atomic non-distinct: Natural [1,32]
|#

#| PROBLEM:
Using the LetterGrade data definition below, design a function that
consumes a letter grade and produces the next highest letter grade.
|#

;; LetterGrade is one of:
;;  - "A"
;;  - "B"
;;  - "C"
;; interpretation: The letter grade in a course
;; <examples are redundant for enumerations>

#| Template: LetterGrade

(defun fn-for-lettergrade (lg)
  (cond ((string-equal lg "A") lg)
	((string-equal lg "B") lg)
	((string-equal lg "C") lg)))

Rules used:
one of: 3 cases
- atomic distinct: "A"
- atomic distinct: "B"
- atomic distinct: "C"

|#


;;; =====================================================================
;;;; FUNCTION DEFINITIONS

;; TrafficLight -> TrafficLight
(defun traffic-light-next (state)
  "Yields the next state given the current state"
  (cond ((string= "red" state) "green")
	((string= "green" state) "yellow")
	((string= "yellow" state) "red")))

;; aisle-seat-p : SeatNum -> Boolean
;; (defun aisle-seat-p (seatnum) nil)	; stub
;; <use template from SeatNum>
(defun aisle-seat-p (seatnum)
  "Produces T if the given seat number is on the aisle; Otherwise NIL.
given: 1, expect: T
given: any number from 2 - 29, expect: NIL
given: 32, expect: T"
  (or (= seatnum 1) (= seatnum 32)))

;; middle-seat-p : SeatNum -> Boolean
(defun middle-seat-p (seatnum)
  "Produces T if the given seat number is not an aisle seat; Otherwise NIL."
  (funcall (complement #'aisle-seat-p) seatnum))

;; bump-up : LetterGrade -> LetterGrade
;; (defun bump-up (lg) "A") ; stub
;; <use template from LetterGrade>
(defun bump-up (lg)
  "Produces next highest letter grade. No change for A.
given: \"C\", expect: \"B\"
given: \"B\", expect: \"A\"
given: \"A\", expect: \"A\""
  (cond ((string-equal lg "B") "A")
	((string-equal lg "C") "B")
	(t lg)))
