;;;; test.lisp

(in-package :areas)

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

(defparameter *r* (make-instance 'rectangle :length-1 10 :length-2 20))

(perimeter *r*) => 60

(setf (perimeter *r*) 30)
*r*             => #rectangle(a=5 b=10)



(+
 (equivalent-area-group-holes
  (* 22. 0.8 (area (make-instance 'circle :radius 1.5)))
  (* 29. 0.8 (area (make-instance 'circle :radius 1.))))
;;;; 62.88725429236749d0

 (equivalent-area-group-holes
  (* 25. 0.8 (area (make-instance 'circle :radius 1.5)))
  (* 31. 0.8 (area (make-instance 'circle :radius 1.))))
;;;; 68.23525661357998d0

 (equivalent-area-group-holes
  (*  1. 0.8 (area (make-instance 'circle :radius 13/2)))
  (* 11. 0.8 (area (make-instance 'circle :radius 1.)))
  (*  9. 0.8 (area (make-instance 'circle :radius 1.))))

;;;; 17.273322937009457d0

 (equivalent-area-group-holes
  (*  1. 0.8 (area (make-instance 'circle :radius 13/2)))
  (* 11. 0.8 (area (make-instance 'circle :radius 1.)))
  (*  9. 0.8  (area (make-instance  'circle :radius 1.))))
;;;; 17.273322937009457d0

 (* 5
    (equivalent-area-group-holes
     (*  2. 0.8  (area (make-instance 'circle :radius 8/2)))
     (* 14. 0.8  (area (make-instance 'circle :radius 0.8)))))
;;;; 108.42462773141749d0
 )

;;;; 274.0937845113839d0

(/ 274.093 (+ 274.093 4800.))

(/
 (- (area (make-instance 'circle :radius 80))
    (area (make-instance 'circle :radius 76)))
(area (make-instance 'circle :radius 80)))

