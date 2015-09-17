;;;; areas.lisp

(in-package #:areas)

;;; "areas" goes here. Hacks and glory await!

(defun f_otv(d)
  "Вычисляет площадь отверстия по его диаметру"
  (* d d 0.25 pi))

(defun d_ek(f)
  "Вычисляет диаметр круга по его площади"
  (sqrt (/ (* 4 f) pi)))

(defun f_ek (x &rest rst)
  ""
  (/ (sqrt (apply #'+ (mapcar #'(lambda(el) (/ 1.0 el el)) (cons x rst))))))

(defun dtr (d)
  "Преобразование градусов в радианы."
  (/ d (/ 180.0 pi)))

(defun f_otn(x &rest rst)
  (let ((summ (apply #'+ (cons x rst))))
    (mapcar #'(lambda (el) (/ el summ)) (cons x rst))))

(defclass zavihritel-osevoy ()
  (
   (inner-diameter :accessor zavihritel-osevoy-inner-diameter :initarg :inner-diameter :initform 50.0)
   (outer-diameter :accessor zavihritel-osevoy-outer-diameter :initarg :outer-diameter :initform 100.0)
   (number-lopatok :accessor zavihritel-osevoy-number-lopatok :initarg :number-lopatok :initform 12)
   (angle-lopatok  :accessor zavihritel-osevoy-angle-lopatok  :initarg :angle-lopatok  :initform 50)
   (delta-lopatok  :accessor zavihritel-osevoy-delta-lopatok  :initarg :delta-lopatok  :initform 2.5)))

(defgeneric area (x)
  (:documentation "Вычисляет площадь")
  (:method ((x zavihritel-osevoy))
    "Вычисляет площадь осевого завихрителя
(defparameter aaa (make-instance 'zavihritel-osevoy))
(area aaa)"
    (- 
     (* pi
	1/4
	(cos(dtr (zavihritel-osevoy-angle-lopatok x)))
	(-
	 (*
	  (zavihritel-osevoy-outer-diameter x)
	  (zavihritel-osevoy-outer-diameter x))
	 (*
	  (zavihritel-osevoy-inner-diameter x)
	  (zavihritel-osevoy-inner-diameter x))))
     (* 1/2
	(zavihritel-osevoy-number-lopatok x)
	(zavihritel-osevoy-delta-lopatok x)
	(- (zavihritel-osevoy-outer-diameter x)
	   (zavihritel-osevoy-inner-diameter x))))))

(defclass point_2d()
  ((x :accessor point_2d-x :initarg :x :initform 0.0)
   (y :accessor point_2d-y :initarg :y :initform 0.0)))

(defgeneric print_01 (p)
  (:documentation "Bla bla bla")
  (:method ((p point_2d))
    (format T "#point_2d(x=~S y=~S)" (point_2d-x p) (point_2d-y p))))

(defmethod print-object :before ((p point_2d) s ) (format s "#point_2d" ))
(defmethod print-object         ((p point_2d) s ) (format s "(x=~S y=~S)" (point_2d-x p) (point_2d-y p)))

;;;;(make-instance 'poi)

(defclass areable()
  ( )
  )

(defclass perinmetrable()
  ( )
  )

(defclass circle()
  ()
  )

(defclass rectangle()
  ()
  )

(defclass romb()
 ()  
  )

(defclass circle()
  ()
  )
