;;;; classes.lisp

(in-package :areas)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <point-3d> ()
  ((x :accessor <point-3d>-x :initarg :x :initform 0.0)
   (y :accessor <point-3d>-y :initarg :y :initform 0.0)
   (z :accessor <point-3d>-z :initarg :z :initform 0.0)))

(defmethod print-object :before ((x <point-3d>) s) (format s "#<point-3d>" ))

(defmethod print-object ((point-3d <point-3d>) s)
  (format s "~S"
	  (list (<point-3d>-x point-3d)
                (<point-3d>-y point-3d)
                (<point-3d>-z point-3d))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <areable> () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <perimetrable> () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <circle> (<areable> <perimetrable>)
  ((radius :accessor <circle>-radius :initarg :radius :initform 100.0 :documentation "Радиус окружности.")
   (center :accessor <circle>-center :initarg :center :initform (make-instance '<point-3d>) :documentation "Радиус окружности."))
  (:documentation "@b(Описание:) класс @b(<circle>) представляет круг."))

(defmethod print-object :before ((x <circle>) s) (format s "#<circle>" ))

(defmethod print-object         ((x <circle>) s)
  (format s "(r=~A c=~S)"
          (<circle>-radius x)
          (<circle>-center x)))

(defclass <circular-segment> (<circle>)
  ((angle :accessor <circular-segment>-angle :initarg :angle :initform pi :documentation "Центральный угол сегмента."))
  (:documentation "@b(Описание:) класс @b(<circle>) представляет сегмент круга."))

(defmethod print-object         ((x <circular-segment>) s)
  (format s "(r=~A c=~S α=~A)"
          (<circle>-radius x)
          (<circle>-center x)
          (<circular-segment>-angle x)))

(defmethod width ((circular-segment <circular-segment>))
  
  )

(defmethod height ((x <circular-segment>))
  "@b(Описание:) метод @b(height) возвращает высоту кругового сегмента.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (defparameter *circular-segment* (make-instance '<circular-segment>))
 (setf (height *circular-segment*) 100.0) => 3.1415927
@end(code)"
  (* (<circle>-radius x)
     (- 1 (cos (/ (<circular-segment>-angle x)
                  2)))))

(defmethod (setf height) (height (x <circular-segment>))
  "@b(Описание:) метод @b(height) устанавливант высоту кругового сегмента.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (defparameter *circular-segment* (make-instance '<circular-segment>))
 (setf (height *circular-segment*) 50.0) => 2.0943952
@end(code)"
  (setf (<circular-segment>-angle x)
        (* 2
           (acos (- 1 (/ height
                  (<circle>-radius x)))))))

(defmethod area ((x <circular-segment>))
  "@b(Описание:) метод @b(area) возвращает площадь кругового сегмента.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (defparameter *circular-segment* (make-instance '<circular-segment>))
 (setf (height *circular-segment*) 10.0)
 (area *circular-segment*)
@end(code)"
  (* (<circle>-radius x)
     (<circle>-radius x)
     (- (<circular-segment>-angle x)
        (sin (<circular-segment>-angle x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass <form> () ())

;;;; @export
;;;; (defmethod html-out ((x <circle>) )
;;;;   (cl-who:with-html-output-to-string (output nil :prologue t :indent t)
;;;;     (:form
;;;;      :action "edit"
;;;;      (:table
;;;;       (:tr (:td "Radius")       (:td (:input :type "number" :name "circle[radius]"   :value (cl-who:str (circle-radius x)))))
;;;;       (:tr (:td "Diameter")     (:td (:input :type "number" :name "circle[Diameter]" :value (cl-who:str (diameter      x)))))
;;;;       (:tr (:td "Area"          (:td (:input :type "number" :name "circle[Area]"     :value (cl-who:str (coerce (area x) 'short-float)))))))  ;; 
;;;;      (:input :type "submit"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <rectangle> (<areable> <perimetrable>)
  ((length-1 :accessor <rectangle>-length-1 :initarg :length-1 :initform 100.0
	     :documentation "Длина первой стороны прямоунольника")
   (length-2 :accessor <rectangle>-length-2 :initarg :length-2 :initform 50.0
	     :documentation "Длина второй стороны прямоунольника")
   (center   :accessor <rectangle>-center   :initarg :center :initform (make-instance '<point-3d>)
	     :documentation "Геометрический центр прямоугольника")
   (angle    :accessor <rectangle>-angle    :initarg :center :initform 0
	     :documentation "Направление первой стороны в радианах"))
  (:documentation "@b(Описание:) класс @b(<circle>) представляет круг."))

(defmethod print-object :before ((x <rectangle>) s) (format s "#<rectangle>" ))

(defmethod print-object         ((x <rectangle>) s)
  (format s "(a=~A b=~A)"
          (<rectangle>-length-1 x)
          (<rectangle>-length-2 x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <rhombus> (<areable> <perimetrable>) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <swirler-axial> (<areable> <perimetrable>)
  ((out-diameter :accessor <swirler-axial>-out-diameter :initarg :out-diameter :initform 100.0 :documentation "Наружный диаметр лопаточного аппарата")
   (in-diameter  :accessor <swirler-axial>-in-diameter  :initarg :in-diameter  :initform 50.0  :documentation "Внутренний диаметр лопаточного аппарата")
   (vane-number  :accessor <swirler-axial>-vane-number  :initarg :vane-number  :initform 12    :documentation "Количество лопаток завихрителя")
   (vane-angle   :accessor <swirler-axial>-vane-angle   :initarg :vane-angle   :initform 50    :documentation "Угол установки лопаток завихрителя, градусы")
   (vane-width   :accessor <swirler-axial>-vane-width   :initarg :vane-width   :initform 2.5   :documentation "Толщина лопаток завихрителя"))
  (:documentation "Представляет осевой завихритель"))

(defmethod print-object :before ((x <swirler-axial>) s) (format s "#<swirler-axial>" ))

(defmethod print-object         ((x <swirler-axial>) s)
  (format s "(do=~A di=~A vn=~A va=~A vw=~A)"
	  (<swirler-axial>-out-diameter x)
          (<swirler-axial>-in-diameter  x)
          (<swirler-axial>-vane-number  x)
          (<swirler-axial>-vane-angle   x)
          (<swirler-axial>-vane-width   x)))

(defmethod area ((x <swirler-axial>))
  "@b(Описание:) метод @b(area) вычисляет площадь осевого завихрителя

 @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((aaa (make-instance '<swirler-axial>)))
    (print  (list aaa (area aaa)))) 
@end(code)
"
  (- (* pi 1/4 (cos (degrees->radians (vane-angle x)))
	(- (* (out-diameter x) (out-diameter x))
	   (* (in-diameter x) (in-diameter x))))
     (* 1/2 (vane-number x) (vane-width x) (- (out-diameter x) (in-diameter x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <pnt-curve> ()
  ((points :accessor <pnt-curve>-points :initarg :points :initform nil :documentation "Задает список точек."))
  (:documentation "Представляет 2d-кривую, заданную точками. У этой
 кривой абсциссы точек должны монотонно возрастать или убывать."))

(defmethod print-object ((x <pnt-curve>) s)
  (format s "#<pnt-curve> (~{~S~^ ~})" (<pnt-curve>-points x)))

(defmethod y-by-x ((pnt-curve <pnt-curve>) x)
  "@b(Описание:) метод @b(y-by-x) возвращает ординату по значению
  абсциссы для кривой @b(pnt-curve)."
  (math/appr:appr-table x (<pnt-curve>-points pnt-curve)))

(defmethod integrate ((pnt-curve <pnt-curve>) (from number) (to number) &key (step 0.001))
  "@b(Описание:) метод @b(integrate) возвращает площадь области
  ограниченной: кривой @b(pnt-curve), осью абсцисс y=0, кривой
  x=@b(from), кривой x=@b(to). Интенрирование выполняется по формуле
  Ньютона-Симпсона. Шаг интегрирования step=0.001."
  (labels ((y (x) (y-by-x pnt-curve x)))
    (let ((n (/ (- to from) step)))
      (when (< n 10) (setf n 10))
      (let  ((delta (/ (- to from) n)))
        (apply #'+
               (loop :for i :from 0 :below n
                     :collect
                     (let ((a (+ from (* delta i)))
                           (b (+ from (* delta (1+ i)))))
                       (simpson a b #'y))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

