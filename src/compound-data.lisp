;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: htdc -*-
;;;
;;; How to Design Compound Data
;;; =====================================================================

(defpackage htdc
  (:documentation "How to Design Compound Data with accompanying examples
and problem sets.")
  (:use :cl)
  (:export
   ;; JetFighter
   #:within-range
   #:reduce-range
   #:time->seconds
   #:jet-fighter #:make-jet-fighter #:copy-jet-fighter #:jet-fighter-p
   #:jet-fighter-designation
   #:jet-fighter-acceleration
   #:jet-fighter-top-speed
   #:jet-fighter-range

   ;; Time
   #:time->seconds
   #:time #:make-time #:copy-time #:time-p #:time-hours #:time-minutes
   #:time-seconds

   ;; Shapes
   #:distance-to-0 #:perimeter #:area
   #:posn #:make-posn #:posn-p  #:copy-posn #:posn-x #:posn-y

   ;; Circles
   #:circle
   #:make-circle #:copy-circle #:circle-p #:circle-center #:circle-radius

   ;; Squares
   #:square
   #:make-square #:copy-square #:square-p #:square-nw #:square-width
   ))

(in-package :htdc)

;;; =====================================================================
;;;; DATA DEFINITIONS:

(defparameter *distance* 0
  "Distance is a Number.
Interpreted as Range from Airbase to target in miles")

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

#| TEMPLATE (JetFighter):

;; fn-for-jet-fighter : JetFighter -> ???
(defun fn-for-jet-fighter (jf)
  "Template rules used:
- compound: 4 fields"
  (list (jet-fighter-designation jf) 	; Symbol
	(jet-fighter-acceleration jf) 	; Number
	(jet-fighter-top-speed jf) 	; Number
	(jet-fighter-range jf))) 	; Number

|#

;; Movie is a structure of
;;   (make-movie String  String)
;; INTERPRETATION: <see MOVIE documentation>
(defstruct movie
  "Where 
  :TITLE is a String representing the movie's name
  :PRODUCER is a String of the Producer's name."
  title producer)

#| TEMPLATE (Movie):

;; fn-for-movie : Movie -> ???
(defun fn-for-movie (film)
  (values (movie-title film)
	  (movie-producer film)))

|#

;; Girlfriend is a structure:
;;   (make-girlfriend String Symbol Symbol Symbol)
;; INTERPRETATION: <see Girlfriend documentation>
(defstruct girlfriend
  "Contains the details of a girlfriend, where
:NAME is a String of the full name and 
:HAIR and :EYES are Symbols used to describe colours.
:PHONE is also a Symbol "
  name hair eyes phone)

#| TEMPLATE (Girlfriend):

;; fn-for-gf : Girlfriend -> ???
(defun fn-for-gf (gf)
  (values (girlfriend-name gf)
	  (girlfriend-hair gf)
	  (girlfriend-eyes gf)
	  (girlfriend-phone gf)))
|#

;; Cheerleader is a structure:
;;   (make-cheerleader String Symbol)
;; INTERPRETATION: <see CHEERLEADER documentation
(defstruct cheerleader
  "Contains a Cheerleaders contact details, where
:NAME is a String of the full name and 
:NUMBER is a Symbol representing her contact. "
  name number)

#| TEMPLATE (Cheerleader): 

;; fn-for-cheerleader : Cheerleader -> ???
(defun fn-for-cheerleader (cl)
  (values (cheerleader-name cl)
	  (cheerleader-number cl)))

|#

;; CD is a structure:
;;   (make-cd String String Single-Float)
;; INTERPRETATION: <see CD documentation>
(defstruct cd
  "Represtents the information pertaining to a Compact-Disc,
where :ARTIST and :TITLE are Strings and
:PRICE is a Single-Float of the CD's retail."
  artist title price)

#| TEMPLATE:

;; fn-for-cd : CD -> ???
(defun fn-for-cd (cd)
  (values (cd-artist cd)
	  (cd-title cd)
	  (cd-price cd)))

|#

;; Sweater is a structure:
;;   (make-sweater Symbol Symbol String)
;; INTERPRETATION: <see SWEATER documentation>
(defstruct sweater
  "Details of a sweater, where :MATERIAL and :SIZE are Symbols and
:PRODUCER is a String"
  material size producer)

#| TEMPLATE:

;; fn-for-sweater : Sweater -> ???
(defun fn-for-sweater (a-sweater)
  (values (sweater-material a-sweater)
	  (sweater-size a-sweater)
	  (sweater-producer a-sweater)))

|#

;; A Time is a structure:
;;   (make-time Number Number Number)
(defstruct time
  "Represents points in time since midnight,
 where :HOURS :MINUTES and :SECONDS are numbers.
:HOURS does not exceed 23 (can be limited to 12); whereas :MINUTES
and :SECONDS do not exceed 59"
  (hours 0) 
  (minutes 0)
  (seconds 0))

#| TEMPLATE:

;; fn-for-time : Time -> ???
(defun fn-for-time (a-time)
  (values (time-hours a-time)
	  (time-minutes a-time)
	  (time-seconds a-time)))

|#

;; Words is a structure:
;;   (make-words Symbol Symbol Symbol)
(defstruct words
  "Represents three-letter words. Each symbol is a letter
of the word from 'A to 'Z."
  first-letter second-letter last-letter)


;; A POSN is a struture
;;  (make-posn Number Number)
(defstruct posn 
  "Represents the Cardinal points on an X,Y axis"
  x y)

;; TEMPLATE:
#+(and)
;; fn-for-posn : posn -> ???
(defun fn-for-posn (coord)
  (values (posn-x coord)
	  (posn-y coord)))

;; A Pixel is either:
;; - Number, or
;; - Posn structure.

;; A Circle is a structure:
;;  (make-circle p s)
;; where P is a Posn and S a Number
(defstruct circle 
  center radius)

;; TEMPLATE:
#+(and)
;; f-for-circle : Circle -> ???
(defun f-for-circle (c)
  (values
   (fn-for-posn (circle-center c))
   (circle-radius c)))

;; A Square is a structure:
;;   (make-square p s)
;; where P is a Posn and S a Number
(defstruct square
  nw width)

;; TEMPLATE:
#+(and)
;; f-for-square : Square -> ???
(defun f-for-square (sq)
  (values
   (fn-for-posn (square-nw sq))
   (square-width sq)))

;; A Shape is either:
;; - A Circle or
;; - A Square

;; TEMPLATE:
#+(and)
;; fn-for-shape : Shape -> ???
(defun fn-for-shape (shape)
  (typecase shape
    (circle (f-for-circle shape))
    (square (f-for-square shape))))
					
;;; ===================================================================== 
;;;; FUNCTIONS:				
					
;; JetFighter Distance -> Boolean	
;; (defun within-range (jf distance) NIL) ; stub
;; <template used from JetFighter>	
					
(defun within-range (jf distance)	
  "Consumes a fighter record and the distance of a target from the (fighter's) base.
It determines whether the fighter can reach the intended target. Returns T if it can; Otherwise NIL."
  (> (jet-fighter-range jf) distance))

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

;; Time -> Number
;; Consumes TIME and produces the number of seconds since midnight
;; (defun time->seconds (a-time) (declare (ignore a-time)) 0) ; stub
;; <template from TIME>

;; time->seconds : Time -> Number
(defun time->seconds (a-time)
  "Returns the current TIME as a number of seconds since midnight.

EXAMPLES:
given: 12hrs 30mins 2secs, expect: 45002
given: 1hr 30mins, expect: 5400
given: 1hr, expect: 3600
given: 1min, expect: 60
given: 1sec, expect: 1

SIDE EFFECTS:
None. Time remains unmodified"
  (+ (* (time-hours a-time) 60 60)
     (* (time-minutes a-time) 60)
     (time-seconds a-time)))

;; distance-to-0: Pixel -> Number
;; To compute the distance of A-PIXEL to the origin
;; (defun distance-to-0 (a-pixel) (declare (ignore a-pixel)) 0) ; stub
;; <template from POSN>

(defun distance-to-0 (coord)
  "To compute the distance of A-PIXEL to the origin.

EXAMPLES:
given: 1, expect: 1
given: (make-posn :x 5 :y 2), expect: 5.3851647

SIDE EFFECTS:
None."
  (typecase coord
    (number coord)
    (posn (sqrt (+ (expt (posn-x coord) 2)
		   (expt (posn-y coord) 2))))))

;; perimeter : Shape -> Number
;; (defun perimeter (shape) 0) ; stub
;; <template from POSN>

(defun perimeter (shape)
  "To compute the perimeter of a shape"
  (typecase shape
    (circle (* (* 2 (circle-s shape))
	       pi))
    (square (* (square-s shape) 4))))

;; area : Shape -> Number
;; (defun area (shape) (declare (ignore shape)) 0) ; stub
;; <template from SHAPE>

(defun area (shape)
  "Consumes a shape and computes the area.
EXAMPLES (Circle) :
given: 5, expect: 78.5 

SIDE EFFECTS:
None."
  (typecase shape
    (circle (* pi (expt (circle-s shape) 2)))
    (square (expt (square-s shape) 2))))
