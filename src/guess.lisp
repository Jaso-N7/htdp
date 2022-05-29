(in-package :htdp)

(export '(*perfect* *small* *high*))

(defparameter *perfect* '(perfect)
  "User guessed correctly")
(defparameter *small* '(too small)
  "User guessed too low")
(defparameter *high* '(too large)
  "User guessed too high")

(let ((target (random 99999)))
  
  ;; Number Number -> (Symbol)
  ;; "Consumes two numbers, GUESS and TARGET. Player tries to guess the target."
  ;; (defun check-guess (guess target) '(too small)) ; stub

  ;; TEMPLATE:
  #+(or)
  (defun check-guess (guess target)
    (cond ((= guess target) (list guess))
	  ((< guess target) (list guess))
	  ((> guess target) (list guess))))

  (defun check-guess (guess &optional (target target))
    "Consumes two numbers, GUESS and TARGET. Player tries to guess the target."
    (cond ((= guess target) *perfect*)
	  ((< guess target) *small*)
	  ((> guess target) *high*)))

  (defun reveal-target ()
    "Only used for confirmation..."
    target))

;; Function Number -> (Symbol)
(defun guess-with-tui (fn &optional (chances 5))
  "Starts the guessing game, giving the user some chances to guess
correctly"
  (guesser fn (funcall fn -1) chances 0))

;; Function (Symbol) Number Number -> (Symbol)
(defun guesser (fn response chances  accum)
  "Auxilliary function for GUESS-WITH-TUI"
  (princ response)
  (if (or (= accum chances)
	  (equal response '(perfect)))
      `(target was ,(reveal-target))
      (let* ((ans (read-line
		   (format t "~&This is try ~A/~A, what is your best guess? "
			   (1+ accum)
			   chances)))
	     (resp (funcall fn (parse-integer ans) (reveal-target))))
	(guesser fn resp chances (1+ accum)))))
