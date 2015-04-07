;Joseph Sarabia
;lab 3
;11/21/13
;CAP 4630

(defvar rules nil)
(defvar dbf nil)

(setq rules '(
	      (R1 (&and (a b c))	(x))
	      (R2 (&and (a v c))	(y))
	      (R3 (&and (a v m))	(y))
	      (R4 (&and (a v m))	(x))
	      (R5 (&and (t p)) 		(a))
	      (R6 (&and (w m))		(a))
	      (R7 (&and (a b))		(c))
	     )
)


(setq dbf '(b m w))

(defun checkdb(x db)
 	(cond
	 ((null db)		NIL)
	 ((eq x (car db))	T)
	 (T			(checkdb x (cdr db)))
	 )
)

(defun genlist(x r)
	(cond 
	 ((null r)	nil)
	 ;if the rule matches add that list to the accumulator variable
	 ((eq x (car (car (cdr (cdr (car r))))))	(cons (cons (car (car r)) (car (cdr (car r)))) (genlist x (cdr r))))
	 (T						(genlist x (cdr r)))
	)
)

(defun printgiven(x)
 	(print (list x 'given))
	T
)

(defun printproved(x)
 	(print (list 'Proved x))
	 T
)

(defun printfailed(x)
 	(print (list 'fail x))
	NIL
)

(defun printproving(x)
 	(print (list 'proving-goal x))
	(cond 
	((prove x)		T)
	 ; ((prove x)		(printproved x))
	(T			NIL))
	 ;(T			(printfailed x)))
)

;if the first element is proved, then keep going till one fails
(defun trying-rule(x y)
 	(cond
	 ((null x)			(printfailed y))
	 ((printproving	(car x))	(cond
		 			((null (cdr x))		(printproved y))
					(T			(trying-rule (cdr x) y))
					))
	 (T				(printfailed y))
	)
)

;try first rule, if false try the rest of the rules until the list runs out or finds a true one
(defun l1-or-tree(x)
	(cond
	 ((null x)		NIL)
	 ((printtry (car x))	T)
	 (T			(l1-or-tree (cdr x)))
	 ) 
)
;print the try function
(defun printtry(x)
	(print (list 'trying-rule x))
       	(cond
	 ;just pass the letters from the rule to try, along with the rule itself
 	((trying-rule (car (cdr (cdr x)))  x)			T)
	(T							NIL)
	)
)	

;print out function then call it	  
(defun printl1(x)
 	(print (list 'l1-or-tree x))
	(cond
	 ((l1-or-tree x)			T)
	 (T					NIL)
	)
)

;check if it's in the dbf, otherwise pass to l1-or-tree
;if l1-or-tree returns true, then return true, otherwise return nil
(defun prove(x)

 	(cond
	 ((checkdb x dbf)			(printgiven x))
	 ((printl1 (genlist x rules))		T)
	 (T					NIL)
	)
)

