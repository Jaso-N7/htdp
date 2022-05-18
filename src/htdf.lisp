;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: htdf -*-
;;; Ref: https://htdp.org/2022-2-9/Book/part_preface.html#%28part._sec~3asystematic-design%29
;;; 
;;; 1. From Problem Analysis to Data Definitions
;;; 2. Signature, Purpose Statement, Header
;;; 3. Functional Examples
;;; 4. Function Template
;;; 5. Function Definition
;;; 6. Testing
;;;
;;;
;;; TESTS:
;;; To run the tests, (htdf:test-functions)
;;; 
;;;; How to Design Functions Recipe

(defpackage htdf
  (:documentation "Example of implementing the How to Design Functions recipe.
Ref: https://htdp.org/2022-2-9/Book/part_one.html#%28part._ch~3ahtdp%29
Section: 3.1")
  (:use :cl :ptester)
  (:export
   :double 
   :pluralize
   :string-first
   :string-last
   :string-rest 
   :string-remove-last
   :profit
   :revenue
   :cost
   :wage
   :attendees))

(in-package :htdf)

;; =====================================================================
;;;; FUNCTION DEFINITIONS

#| ======= PROBLEM: =======
Design a function that consumes a number and produces twice that number. ;
Call your function double. Follow the HtDF recipe and show the stub and ;
template				;
===========================|#

;; Number -> Number
;; produce 2 times the given number
;; (defun double (n) 0)	; this is the stub

;; (defun double (n) n) ; this is the template

(defun double (n) 
  "Consume a number N and produce 2 times the given number"
  (* 2 n))

#| ====== PROBLEM: =======
Design a function that pluralizes a given word. 
(Pluralize means to convert the word to its plural form.)
For simplicity you may assume that just adding s is enough to pluralize a word. 
==========================|#

;; String -> String
;; Produce the given string with "s" added to the end
;; (defun pluralize (str) "")	; this is the stub
;; (defun pluralize (str) str)  ; template

(defun pluralize (str)
  "Produce the given string with \"s\" added to the end
given: \"dog\", expect: \"dogs\"
given: \"grass\", expect: \"grasss\""
  (format nil "~As" str))

;; String -> Char
;; Extracts the first character from a non-empty string.
;; (defun string-first (str) #\c) ; stub
;; (defun string-first (str) str) ; template

(defun string-first (str)
  "Extracts the first character from a non-empty string.
given: \"character\", expect: #\c
given: \"String\", expect: #\S"
  (char str 0))

;; String -> Character
;; Extracts the last character from a non-empty string
;; (defun string-last (str) #\r) ; stub
;; (defun string-last (str) str) ; template

(defun string-last (str)
  "Extracts the last character from a non-empty string.
given: \"character\", expect: #\r
given: \"String\", expect: #\g"
  (char str (1- (length str))))

;; String -> String
;; Produces a string with the first character removed.
;; (defun string-rest (str) "haracter")	; stub
;; (defun string-rest (str) str) ; template

(defun string-rest (str)
  "Produces a string with the first character removed.
given: \"character\", expect: \"haracter\"
given: \"String\", expect: \"tring\""
  (subseq str 1))

;; String -> String
;; Produces a string like the given one, with the LAST character removed
;; (defun string-remove-last (str) "Strin") ; stub
;; (defun string-remove-last (str) str) ; template

(defun string-remove-last (str)
  "Produces a string like the given one, with the LAST character removed.
given: \"Cats\", expect: \"Cat\"
given: \"Dogs\", expect: \"Dog\""
  (let ((end (1- (length str))))
    (subseq str 0 end)))
					

#| ======= PROBLEM (HtDP1e Section 3) =======
Imagine the owner of a movie theater who has complete freedom in setting ticket prices. The more he charges, the fewer the people who can afford tickets. In a recent experiment the owner determined a precise relationship between the price of a ticket and average attendance. At a price of $5.00 per ticket, 120 people attend a performance. Decreasing the price by a dime ($.10) increases attendance by 15. Unfortunately, the increased attendance also comes at an increased cost. Every performance costs the owner $180. Each attendee costs another four cents ($0.04). The owner would like to know the exact relationship between profit and ticket price so that he can determine the price at which he can make the highest profit.
==============================================|#

;; profit : Number -> Number
;; (defun profit (ticket-price) 0) ;stub
;; (defun profit (ticket-price) 
;;   ticket-price) ; template

(defun profit (ticket-price)
  "To compute the profit as the difference between revenue and costs
at some given TICKET-PRICE."
  (- (revenue ticket-price)
     (cost ticket-price))) 	

;; revenue : Number -> Number
;; (defun revenue (ticket-price) 0)  ; stub
;; (defun revenue (ticket-price) ticket-price) 	; template

(defun revenue (ticket-price)
  "To compute the revenue, given the ticket-price"
  (* ticket-price
     (attendees ticket-price)))

;; cost : Number -> Number
;; (defun cost (ticket-price) 0) ; stub
;; (defun cost (ticket-price) ticket-price) ; template

(defun cost (ticket-price)
  "To compute the costs, given the ticket-price."
  (+ 180 (* .04 (attendees ticket-price))))

;; attendees : Number -> Number
;; (defun attendees (ticket-price) 0) ; stub
;; (defun attendees (ticket-price) ticket-price) ; template

(defun attendees (ticket-price)
  "To compute the number of attendees, given ticket-price"
  (+ 120
     (* (/ 15 0.1)
	(- 5 ticket-price))))

#| ======= PROBLEM (HtDP1ed Section 4) =======
Company XYZ & Co. pays all its employees $12 per hour. A typical employee works between 20 and 65 hours per week. Develop a program that determines the wage of an employee from the number of hours of work, *if the number is within the proper range.*
============================================ |#

;; wage : Number -> Number
;; (defun wage (hours) 0) ; stub
;; (defun wage (hours) hours) ; template

(defun wage (hours &optional (rate 12))
  "Determines the wage of an employee from the number of hours worked within
a specific range"
  (* rate 
     (cond ((and (>= hours 20) (<= hours 65))
	    hours)
	   ((= hours 20) hours)
	   ((= hours 65) hours)
	   (t  0))))
