;; -*- Mode: Lisp; Syntax: Common Lisp; Package: zoo -*-
;; 
;;; HtDP 1e Exercise 7.2.1

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

;; A Spider is a structure:
;;   (make-spider :legs Number :space Number)
;; Interpretation: <See Documentation>
(defstruct spider
  ":LEGS are the number of remaining legs (we assume that spiders can lose legs
in accidents) and the
:SPACE they need in case of transport."
  (legs 8)
  space)

;; Examples: See Tests
;; fn-for-spider : Spider -> ???
(defun fn-for-spider (s)
  (values (spider-legs s)
	  (spider-space s)))

;; An Elephant is a structure:
;;   (make-elephant :space Number)
;; Interpretation: <See Documentation>
(defstruct elephant
  "An elephants only attribute are the :SPACE they need in case of transportation."
  space)

;; Examples: See Tests

;; fn-for-elephant : Elephant -> ???
(defun fn-for-elephant (e)
  (elephant-space e))

;; A Monkey is a structure:
;;   (make-monkey :intelligence Number :space Number)
;; Interpretation: See documentation
(defstruct monkey
  ":INTELLIGENCE is a number between 0-100 and :SPACE needed for transportation."
  intelligence
  space)

;; fn-for-monkey : Monkey -> ???
(defun fn-for-monkey (m)
  (values (monkey-intelligence m)
	  (monkey-space m)))

;; =============================================================================
;;;; FUNCTIONS
