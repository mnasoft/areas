(require :areas)
(use-package :areas)

(asdf:oos 'asdf:load-op :postmodern)
(use-package :postmodern)

(connect-toplevel "namatv" "namatv" "" "localhost")


(defun foo (name-like-lst)
  (let
      ((lk-lst (cons :and (mapcar #'(lambda (el) (list :ilike 'name (concatenate 'string "%" el "%")) ) 
				  name-like-lst))
	       )
       )
    (doquery (list :select 'designation 'name :from 'gost
		      :where  lk-lst))
	     (designation name)
	     (format t "<tr><td>~A</td><td>~A</td><tr>~%" designation name))))

(defun foo (name-like-lst)
  (let
      ((lk-lst
	(cons :and
	      (mapcar #'(lambda (el) (list :like (quote 'name) (concatenate 'string "%" el "%")) ) 
		      name-like-lst))
	 )
       (assa "%asd%")
       )
    (break "~S" lk-lst )
    (sql (:select 'designation 'name :from 'gost :where lk-lst))))

    (sql (:select 'designation 'name :from 'gost :where (:and (:like 'name (concatenate 'string "%" "el" "%")) (:like 'name "asd"))))


(defun assa()
  (do ((ex-cond nil)
       (lst nil)
       (n nil)
       (d nil)
       (prev-key nil)
       (add nil)
       )
      ((equal ex-cond 'quit) 'done)
      (format t "~S~%" lst)
      (format t "n=~A d=~A~%" n d)
      (format t "Add Subtsract Eval Number Diameter Quit:" lst)
      (setf ex-cond (read ))
      (cond
       ((equal 'Q ex-cond) (setf ex-cond 'quit))
       ((equal 'D ex-cond) (format t "Enter D:")
	(if (numberp(setf ex-cond (read )))
	    (setf D ex-cond)
	  (format t "Value mast be a number.~%")))
       ((equal 'N ex-cond) (format t "Enter N:")
	(if (numberp(setf ex-cond (read )))
	    (setf n ex-cond)
	  (format t "Value mast be a number.~%")))	
       ((equal 'A ex-cond) (if (and (numberp n )(numberp d))
				 (setf lst (cons (list n d) lst))))
       ((equal 'S ex-cond) (if (and (numberp n )(numberp d))
				 (setf lst (cons (list (* n -1) d) lst))))
       ((equal 'E ex-cond) (if (and (numberp n )(numberp d))
				 (format t "Area(n:~A,d:~A)=~A~%" n d (* n (circle-area-by-diameter  d)))))
       ((numberp  ex-cond)
	(format t "~A~%" (circle-area-by-diameter ex-cond))))))

;;;;(save-lisp-and-die "/home/namatv/f_otv" :executable t :compression t :toplevel 'assa)
