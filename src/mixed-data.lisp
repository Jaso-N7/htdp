;;; -*- Mode: Lisp; Syntax: Common Lisp; Package: mixed -*-
;;; 
;;; HtDP 1e Exercise 7.2.1
;;; ============================================================================

(defpackage mixed
  (:documentation "Collection of Zoo Animals.")
  (:use :cl)
  (:export
   ;; Zoo
   :fits-p
   #:spider #:make-spider #:copy-spider #:spider-p #:spider-legs #:spider-space
   #:elephant #:make-elephant #:copy-elephant #:elephant-p #:elephant-space
   #:monkey #:make-monkey #:copy-monkey #:monkey-p #:monkey-intelligence #:monkey-spacev

   ;; Vehicles
   #:bus #:make-bus #:bus-p #:bus-route #:bus-doors #:bus-capacity #:bus-has-conducter-p
   #:train #:make-train #:train-p #:copy-train

   ;; Speed-Vector
   #:checked-make-vec
   #:speed-vector #:copy-speed-vector #:speed-vector-p
   #:speed-vector-x #:speed-vector-y))

(in-package :mixed)

;; =============================================================================
;;;; DATA DEFINITIONS

;; ======= Zoo Animals

;; An Animal is one of:
;; - Spider
;; - Elephant
;; - Monkey
;; Interpretation: Represents a type of Zoo Animal and it's characteristics.

;; A Cage is a Number
;; Interpretation: Describes the volume of a cage in Cubit Inches

;; A Spider is a structure:
;;   (make-spider :legs Number :space Number)
;; Interpretation: <See Documentation>
(defstruct spider
  ":LEGS are the number of remaining legs (we assume that spiders can lose legs
in accidents) and the
:SPACE they need in case of transport."
  (legs 8)
  space)

;; EXAMPLES: See Tests

;; TEMPLATE:

#+(or)
;; fn-for-spider : Spider -> ???
(defun fn-for-spider (s)
  (values 
   (spider-legs s)
   (spider-space s)))

;; An Elephant is a structure:
;;   (make-elephant :space Number)
;; Interpretation: <See Documentation>
(defstruct elephant
  "An elephants only attribute are the :SPACE they need in case of transportation.
:SPACE is a Number representing Cubit Feet"
  (space 400))

;; EXAMPLES: See Tests

;; TEMPLATE:

#+(or)
;; fn-for-elephant : Elephant -> ???
(defun fn-for-elephant (e)
  (elephant-space e))


;; A Monkey is a structure:
;;   (make-monkey :intelligence Number :space Number)
;; Interpretation: See documentation
(defstruct monkey
  ":INTELLIGENCE is a number between 0-100 and :SPACE needed for transportation."
  intelligence
  (space 20))

;; TEMPLATE:

#+(or)
;; fn-for-monkey : Monkey -> ???
(defun fn-for-monkey (m)
  (values
   (monkey-intelligence m)
   (monkey-space m)))

;; ======= Vehicles

;; A Vehicle is one of:
;; - buses
;; - limos
;; - cars
;; - subways
;; Interpretation: Fleets of vehicles managed by metropolitan transportation
;;                 agencies.

;; A Bus is a structure:
;;   (make-bus route doors capacity has-conducter-p)
;; where: ROUTE is a Symbol, DOORS and PASSENGERS are Numbers and HAS-CONDUCTER-P
;;        is a Boolean.
(defstruct bus
  "Represents a bus and its passenger capacity."
  doors capacity has-conducter-p)

;; TEMPLATE:

#+(and)
;; fn-for-bus : Bus -> ???
(defun fn-for-bus (b)
  (values
   (bus-doors b)
   (bus-capacity b)
   (bus-has-conducter-p b)))

;; A Limo is a structure
;;   (make-limo has-sunroof-p length colour)
;; where: has-sunroof-p is a Boolean, Length is a Number and colour is a Symbol
(defstruct limo
  (has-sunroof-p t)
  length
  colour)

;; TEMPLATE:

#+(and)
;; fn-for-limo : Limo -> ???
(defun fn-for-limo (l)
  (values
   (limo-has-sunroof-p l)
   (limo-length l)
   (limo-colour l)))

;; A Car is a structure:
;;   (make-car type doors colour cc has-turbo-p)
;; where: type is a Symbol of 'sedan 'convertible 'hatch-back
;;        doors is a Number, COLOUR is a Symbol, cc is a Number and has-turbo-p
;;        is a Boolean
(defstruct car
  type doors colour cc
  (has-turbo-p nil))

;; TEMPLATE:

#+(and)
;; fn-for-car : Car -> ???
(defun fn-for-car (c)
  (values
   (car-type c)
   (car-doors c)
   (car-colour c)
   (car-cc c)
   (car-has-turbo-p c)))

;; A Train is a structure:
;;   (make-train route length capacity type has-conducter-p)
;; where: ROUTE is a Symbol, LENGTH and CAPACITY are Numbers, TYPE is a Symbol of
;;        either 'tram, 'bullet, 'steam or 'subway and HAS-CONDUCTER-P is a Boolean
(defstruct train
  route
  length
  capacity
  (type 'subway)
  (has-conducter-p nil))

;; TEMPLATE:

#+(and)
;; fn-for-train : Train -> ???
(defun fn-for-train (train)
  (values
   (train-route train)
   (train-length train)
   (train-capacity train)
   (train-type train)
   (train-has-conducter-p train)))

;; Ex 7.5.3
;; A Speed-Vector is a structure:
;;   (make-speed-vector x y)
;; where both X and Y are positive numbers
(defstruct speed-vector
  x y)

;; TEMPLATE

#+(or)
;; f-for-speed-vec : Speed-Vector -> ???
(defun f-for-speed-vec (v)
  (values
   (speed-vector-x v)
   (speed-vector-y v)))

;; =============================================================================
;;;; FUNCTIONS

;; ======= Zoo Animals

;; fits-p : Animal Cage -> Boolean
;; Determines whether the cage is large enough for the given animal
;; (defun fits-p (animal cage) (declare (ignore animal cage)) nil) ; stub
;; <template taken from various animals>

(defun fits-p (animal cage)
  "With the given Zoo Animal and the volume of the cage, determin whether the
cage is large enough for the animal.
Does a spider take up less space with less legs?"
  (typecase animal
    (spider (> cage (spider-space animal)))
    (elephant (> cage (elephant-space animal)))
    (monkey (> cage (monkey-space animal)))))

;; Ex: 7.5.1
;; area-of-disk : Number -> Float
;; to compute the area of a disk with radius V, if V is a Number
;; (defun area-of-disk (v) 0) ; stub

(defun area-of-disk (v)
  "Given a radius of Number V, compute the area of a disk.
EXAMPLE:
given: 3, expect: 28.274333882308138D0"
  (assert (and  (numberp v) (plusp v))
	  (v) 
	  "Expected a Positive Number")
  (* pi (expt v 2)))

;; EXAMPLES: See Tests

;; Ex: 7.5.3
;; checked-make-vec : Number Number -> Speed-Vector
;; Ensures arguments to MAKE-SPEED-VECTOR are positive numbers
(defun checked-make-vec (x y)
  "An abstraction to make-speed-vector that ensures only the correct
values are used. Once checked, returns a Speed-Vector"
  (assert (and (plusp x)
	       (plusp y))
	  (x y)
	  "Expecting both inputs to be Positive Numbers.")
  (make-speed-vector :x x :y y))
