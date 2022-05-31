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
