;;;; classes.lisp

(in-package :areas)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass areable() ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass perimetrable() ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass circle (areable perimetrable)
  ((radius :accessor circle-radius :initarg :radius :initform 100.0 :documentation "Радиус окружности")
   (center :accessor circle-center :initarg :center :initform (make-instance 'point-3d) :documentation "Радиус окружности")))

(defmethod print-object :before ((x circle) s) (format s "#circle" ))

(defmethod print-object         ((x circle) s) (format s "(r=~A c=~S)" (circle-radius x) (circle-center x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass rectangle()
  ((length-1 :accessor rectangle-length-1 :initarg :length-1 :initform 100.0
	     :documentation "Длина первой стороны прямоунольника")
   (length-2 :accessor rectangle-length-2 :initarg :length-2 :initform 50.0
	     :documentation "Длина второй стороны прямоунольника")
   (center   :accessor rectangle-center   :initarg :center :initform (make-instance 'point-3d)
	     :documentation "Геометрический центр прямоугольника")
   (angle    :accessor rectangle-angle    :initarg :center :initform 0
	     :documentation "Направление первой стороны в радианах")))

(defmethod print-object :before ((x rectangle) s) (format s "#rectangle" ))

(defmethod print-object         ((x rectangle) s) (format s "(a=~A b=~A)" (rectangle-length-1 x) (rectangle-length-2 x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass romb() ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

