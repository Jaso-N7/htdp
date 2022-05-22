;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: htdc -*-
;;;
;;; How to Design Compound Data
;;; =====================================================================

(defpackage htdc
  (:documentation "How to Design Compound Data with accompanying examples
and problem sets.")
  (:use :cl)
  (:export
   #:within-range
   #:reduce-range
   #:make-jet-fighter
   #:jet-fighter-p
   #:jet-fighter-designation
   #:jet-fighter-acceleration
   #:jet-fighter-top-speed 
   #:jet-fighter-range))

(in-package :htdc)

;;; =====================================================================
;;;; DATA DEFINITIONS:

;; Distance is Number
;; Interpretation: Range from base to target in Miles

(defparameter *distance* 0
  "Range from Airbase to target in miles")

;; JetFighter is a structure
;;   (make-jet-fighter Symbol Number Number Distance)
;; Interpretation: See JET-FIGHTER documentation. For ease of computation
;;                 all units are represented in MILES unless otherwise stated.
(defstruct jet-fighter
  "An airforce's jet fighter performance.
Where 
  :DESIGNATION is a Symbol 
  :ACCELERATION and :TOP-SPEED are Numbers representing MPH
  :RANGE is the DISTANCE (in miles) indicative of effective Combat Range."
  designation
  acceleration 				; 
  top-speed
  range)

;;; JetFighter Examples

(defparameter +raptor+ (make-jet-fighter
			:designation 'f-22a
			:acceleration 51.75
			:top-speed 1500
			:range 679)
  "Lockheed Martin Raptor F-22A information taken from various sources.
Acceleration in seconds. Unofficially 3.05s quicker than the requirement of 54s.
Top speed Mach 2.25 at altitude in MPH.
(Combat) Range in supercruise")
(defparameter +tornado+ (make-jet-fighter
			 :designation 'f3
			 :acceleration 25
			 :top-speed 1500
			 :range 1151)
  "Tornado F3 information taken from various sources.
Acceleration at low level the acceleration from 250 to 600 kts took approximately 25 s
Top speed in MPH
(Combat) Range subsonic")
(defparameter +mig23+ (make-jet-fighter
		       :designation 'flogger-k
		       :acceleration 3.45
		       :top-speed 1553
		       :range 930)
  "MiG-23MLD information taken from various sources.
- Acceleration rate at sea level and from 600 km/h to 1100 km/h for the
MiG-23ML reaches 29 km / h  per second. Converting this to acceleration time,
we can say that at the sea level by every 100 km / h of speed increase, 
and the MiG-23ML in 3.45s
- Top speed at altitude.
- (Combat) Range with standard armament, no drop-tanks.")

#| TEMPLATE:

;; fn-for-jet-fighter : JetFighter -> ???
(defun fn-for-jet-fighter (jf)
  "Template rules used:
- compound: 4 fields"
  (list (jet-fighter-designation jf) 	; Symbol
	(jet-fighter-acceleration jf) 	; Number
	(jet-fighter-top-speed jf) 	; Number
	(jet-fighter-range jf))) 	; Number

|#

;;; =====================================================================
;;;; FUNCTIONS:

;; JetFighter Distance -> Boolean
;; (defun within-range (jf distance) NIL) ; stub
;; <template used from JetFighter>

(defun within-range (jf distance)
  "Consumes a fighter record and the distance of a target from the (fighter's) base.
It determines whether the fighter can reach the intended target. Returns T if it can; Otherwise NIL."
  (> (jet-fighter-range jf) distance))
;; Can also be coded: (> (jet-fighter-range jf) distance)

;; JetFighter -> JetFighter
;; (defun reduce-range (jf) jf) ; stub
;; <template used from JetFighter>

(defun reduce-range (jf)
  "Consumes a fighter record and produces one in which the range field is reduced to 80% of the original value.

EXAMPLES:
given: 600, expect: 480

SIDE EFFECTS:
The struct is modified."
  (make-jet-fighter
   :designation (jet-fighter-designation jf) 	; Symbol
   :acceleration (jet-fighter-acceleration jf) 	; Number
   :top-speed (jet-fighter-top-speed jf)	; Number
   :range (* 80/100 (jet-fighter-range jf))))	; Number
