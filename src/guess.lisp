(in-package :htdp)

(let ((target (random 99999)))
  
  ;; Number Number -> (Symbol)
  ;; "Consumes two numbers, GUESS and TARGET. Player tries to guess the target."
  ;; (defun check-guess (guess target) '(too small)) ; stub

  #| TEMPLATE: check-guess
  				       	;
  (defun check-guess (guess target)	;
  (cond ((= guess target) (list guess))	;
  ((< guess target) (list guess))	;
  ((> guess target) (list guess))))	; ; ;
					;
  |#

  (defun check-guess (guess &optional (target target))
    "Consumes two numbers, GUESS and TARGET. Player tries to guess the target."
    (cond ((= guess target) '(perfect))
	  ((< guess target) '(too small))
	  ((> guess target) '(too large))))

  (defun reveal-target ()
    "Only used for confirmation..."
    target)

  ;; Number (Symbol) Number -> (Symbol)
  (defun guess-with-tui (&optional (chances 5) (response '(begin)) (accum 0))
    "Starts the guessing game, giving the user some chances to guess
correctly"
    (princ response)
    (if (or (= accum chances)
	    (equal response '(perfect)))
	`(target was ,(reveal-target))
	(let ((ans (read-line
		    (format t "~&This is try ~A/~A, what is your best guess? "
			    (1+ accum)
			    chances))))
	  (guess-with-tui chances
			  (check-guess (parse-integer ans) (reveal-target))
			  (1+ accum))))))
