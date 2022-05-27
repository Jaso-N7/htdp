;;; -*- Mode: Lisp; Syntax: Common Lisp; Package: zoo -*-
;;; 
;;; HtDP 1e Exercise 7.2.1
;;; ============================================================================

(defpackage zoo
  (:documentation "Collection of Zoo Animals.")
  (:use :cl)
  (:export
   #:fits-p
   #:make-spider
   #:make-elephant
   #:make-monkey
   ))

(in-package :zoo)

;; =============================================================================
;;;; DATA DEFINITIONS

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

#| TEMPLATE : Spider

;; fn-for-spider : Spider -> ???
(defun fn-for-spider (s)
  (values 
   (spider-legs s)
   (spider-space s)))

|#

;; An Elephant is a structure:
;;   (make-elephant :space Number)
;; Interpretation: <See Documentation>
(defstruct elephant
  "An elephants only attribute are the :SPACE they need in case of transportation.
:SPACE is a Number representing Cubit Feet"
  (space 400))

;; EXAMPLES: See Tests

#| TEMPLATE : Elephant

;; fn-for-elephant : Elephant -> ???
(defun fn-for-elephant (e)
  (elephant-space e))

|#

;; A Monkey is a structure:
;;   (make-monkey :intelligence Number :space Number)
;; Interpretation: See documentation
(defstruct monkey
  ":INTELLIGENCE is a number between 0-100 and :SPACE needed for transportation."
  intelligence
  (space 20))

#| TEMPLATE : Monkey

;; fn-for-monkey : Monkey -> ???
(defun fn-for-monkey (m)
  (values
   (monkey-intelligence m)
   (monkey-space m)))

|#

;; =============================================================================
;;;; FUNCTIONS

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


