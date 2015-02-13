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

(defmethod print_01 ((p point_2d))
  (format T "(#point_2d x=~A y=~A)" (point_2d-x p) (point_2d-y p)))

(let ((p1 (make-instance 'point_2d)))
  (print_01 p1))

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
