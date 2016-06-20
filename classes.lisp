;;;; classes.lisp

(in-package :areas)

(defclass zavihritel-osevoy ()
  ((out-diameter :accessor out-diameter :initarg :out-diameter :initform 100.0 :documentation "Наружный диаметр лопаточного аппарата")
   (in-diameter  :accessor in-diameter  :initarg :in-diameter  :initform 50.0  :documentation "Внутренний диаметр лопаточного аппарата")
   (vane-number  :accessor vane-number  :initarg :vane-number  :initform 12    :documentation "Количество лопаток завихрителя")
   (vane-angle   :accessor vane-angle   :initarg :vane-angle   :initform 50    :documentation "Угол установки лопаток завихрителя, градусы")
   (vane-width   :accessor vane-width   :initarg :vane-width   :initform 2.5   :documentation "Толщина лопаток завихрителя"))
  (:documentation "Представляет осевой завихритель"))

(defmethod print-object :before ((x zavihritel-osevoy) s) (format s "#zavihritel-osevoy" ))

(defmethod print-object         ((x zavihritel-osevoy) s) (format s "(do=~A di=~A vn=~A va=~A vw=~A)"
								  (out-diameter x) (in-diameter x) (vane-number x) (vane-angle x) (vane-width x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass point-3d ()
  ((x :accessor x :initarg :x :initform 0.0)
   (y :accessor y :initarg :y :initform 0.0)
   (z :accessor z :initarg :z :initform 0.0)))

(defmethod print-object :before ((x point-3d) s) (format s "#point-3d" ))

(defmethod print-object         ((x point-3d) s) (format s "~S"
							 (list (x x) (y x) (z x))))

(defclass areable() ())

(defclass perinmetrable() ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass circle(areable)
  ((radius :accessor radius :initarg :radius :initform 100.0 :documentation "Радиус окружности")
   (center :accessor center :initarg :center :initform (make-instance 'point-3d) :documentation "Радиус окружности")))

(defmethod print-object :before ((x circle) s) (format s "#circle" ))

(defmethod print-object         ((x circle) s) (format s "(r=~A c=~S)" (radius x) (center x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass rectangle() ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass romb() ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric area (x) (:documentation "Вычисляет площадь"))

(defmethod area ((x zavihritel-osevoy))
    "Вычисляет площадь осевого завихрителя
Пример использования:
;;;; (let ((aaa (make-instance 'zavihritel-osevoy))) (print  (list aaa (area aaa))))
"
  (- (* pi 1/4 (cos (degrees->radians (vane-angle x)))
	(- (* (out-diameter x) (out-diameter x))
	   (* (in-diameter x) (in-diameter x))))
     (* 1/2 (vane-number x) (vane-width x) (- (out-diameter x) (in-diameter x)))))

(defmethod area ((x circle))
  "Возвращает плошадь круга 
Пример использования:
;;;; (let ((c (make-instance 'circle))) (print (list c (area c))))
"
  (* pi (radius x) (radius x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric dxf-out (p &optional s) (:documentation "Выводит объект в dxf"))

(defmethod dxf-out ((p point-3d) &optional (s t))
    (format s "0~%POINT~%10~%~A~%20~%~A~%30~%~A" (x p) (y p) (z p)))

(dxf-out (make-instance 'point-3d) nil)
