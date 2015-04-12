;;;; areas.lisp

(in-package #:areas)

;;; "areas" goes here. Hacks and glory await!

(defun f_otv(d) (* d d 0.25 pi))

;(defun f_ek (x)(/ (sqrt (apply #'+ (mapcar #'(lambda(el) (/ 1.0 el el)) x)))))

(defun f_ek (x &rest rst)
  ""
  (/ (sqrt (apply #'+ (mapcar #'(lambda(el) (/ 1.0 el el)) (cons x rst))))))

(defun dtr (d)
  "Преобразование градусов в радианы."
  (/ d (/ 180.0 pi)))

(defun f_otn(x &rest rst)
  (let ((summ (apply #'+ (cons x rst))))
    (mapcar #'(lambda (el) (/ el summ)) (cons x rst))))

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
