;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: htdc -*-
;;;
;;; How to Design Compound Data
;;; =====================================================================

(defpackage htdc
  (:documentation "How to Design Compound Data with accompanying examples
and problem sets.")
  (:use :cl)
  (:export
   #:make-jet-fighter
   #:jet-fighter-p
   #:jet-fighter-designation
   #:jet-fighter-acceleration
   #:jet-fighter-top-speed 
   #:jet-fighter-range))

(in-package :htdc)

;;; =====================================================================
;;;; DATA DEFINITIONS:

(defstruct jet-fighter
  ;; (:documentation "An airforce's jet fighter performance.
;; Acceleration and Top Speed are MPH
;; Range is indicative of effective Combat Range.")
  designation
  acceleration 				; 
  top-speed
  range)
;; JetFighter is (make-jet-fighter Symbol Number Number Number)
;; Interpretation: See JET-FIGHTER documentation. For ease of computation
;;                 all units are represented in MILES unless otherwise stated.

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

(defun fn-for-jet-fighter (jf)
  "Template rules used:
- compound: 4 fields"
  (list (jet-fighter-designation jf) 	; Symbol
	(jet-fighter-acceleration jf) 	; Number
	(jet-fighter-top-speed jf) 	; Number
	(jet-fighter-range jf))) 	; Number


;;; =====================================================================
;;;; FUNCTIONS:
