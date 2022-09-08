;;;; package.lisp

(defpackage #:areas
  (:use #:cl #:convertion)
  (:export circle-area-by-diameter
           circle-area-by-radius
           circle-diameter-by-area    
           parts
           ring-area
           ring-volume
           ring-mass
           pipe-area
           pipe-volume
           pipe-mass
           round-bar-mass
           )
  (:export equivalent-area-group-holes)
  (:export ring-equal-area-radius
           ring-equal-area-radius-list
           ring-equal-area-radius-relative-higth-list
           ring-equal-area-border-radius-list
           )
  (:export swirler-axial-area)
  (:export <swirler-axial>
           <swirler-axial>-out-diameter
           <swirler-axial>-in-diameter
           <swirler-axial>-vane-number
           <swirler-axial>-vane-angle
           <swirler-axial>-vane-width
           )
  (:export <point-3d>
           <point-3d>-x
           <point-3d>-y
           <point-3d>-z
           )
  (:export <circle>
           <circle>-radius
           <circle>-center
           make-<circle>
           )
  (:export <circular-segment>
           <circular-segment>-angle 
           heigth
           width
           )
  (:export <rectangle>
           <rectangle>-length-1
           <rectangle>-length-2
           <rectangle>-center
           <rectangle>-angle
           )
  (:export <romb>)
  (:export area
           radius
           diameter
           perimeter
           )
  (:export dxf-out
           )
  (:export hydraulic-diameter
           equivalent-diameter
           hydraulic-diameter-rect
           )
  (:export simpson)
  (:export <pnt-curve>
           <pnt-curve>-points
           y-by-x
           integrate))

;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; defgeneric

(in-package :areas)

(defgeneric area   (obj)
  (:documentation "@b(Описание:) area возвращает полщадь объекта."))

(defgeneric (setf area) (new-area obj)
  (:documentation "@b(Описание:) (setf area) устанавливает полщадь объекта."))

(defgeneric perimeter (obj)
  (:documentation "@b(Описание:) perimeter возвращает периметр объекта."))

(defgeneric (setf perimeter) (new-perimeter obj)
  (:documentation "@b(Описание:) (setf perimeter) устанавливает периметр объекта."))

(defgeneric radius (obj)
  (:documentation "@b(Описание:) radius возвращает радиус объекта."))

(defgeneric (setf radius) (new-radius obj)
  (:documentation "@b(Описание:) (setf radius) устанавливает радиус для объекта."))

(defgeneric diameter (obj)
  (:documentation "@b(Описание:) diameter возвращает диаметр объекта."))

(defgeneric (setf diameter) (new-diameter obj)
  (:documentation "@b(Описание:) radius устанавливает диаметр объекта."))

(defgeneric dxf-out (p &optional s) (:documentation "Выводит объект в dxf"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun circle-area-by-diameter (d)
"@b(Описание:) circle-area-by-diameter вычисляет площадь круга по его диаметру.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (circle-area-by-diameter 1) => 0.7853981633974483d0
@end(code)
"
  (* d d 0.25 pi))

(defun circle-area-by-radius (r)
"@b(Описание:) circle-area-by-radius вычисляет площадь круга по его радиусу.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (circle-area-by-radius 1) => 3.141592653589793d0
@end(code)
"
  (* r r pi))

(defun circle-diameter-by-area (a)
"@b(Описание:) circle-diameter-by-area вычисляет диаметр круга по его площади.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (circle-diameter-by-area pi) => 2.0d0
@end(code)
"
  (sqrt (/ (* 4 a) pi)))

(defun equivalent-area-group-holes (x &rest rst)
"@b(Описание:) функция equivalent-area-group-holes вычисляет эквивалентную
площадь последовательно расположенных отверстий.

 @b(Переменые:)
@begin(list)
 @item(x - площадь первого отверстия;)
 @item(rst - перечень площадей последующих отверстий.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (equivalent-area-group-holes 2.0)          => 2.0
 (equivalent-area-group-holes 2.0 2.0)      => 1.4142135
 (equivalent-area-group-holes 2.0 2.0 20.0) => 1.4106913 
 (equivalent-area-group-holes 1.0 2.0 3.0)  => 0.8571428
@end(code)
"
  (/ (sqrt (apply #'+ (mapcar #'(lambda(el) (/ 1.0 el el)) (cons x rst))))))

(defun parts(x &rest rst)
"@b(Описание:) функция parts возвращает список долей, задаваемые перечнем аргументов.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (parts 1.0 2.0 1.0) =>  (0.25 0.5 0.25)
 (parts 1.0)         =>  (1.0)
@end(code)
"
  (let ((summ (apply #'+ (cons x rst))))
    (mapcar #'(lambda (el) (/ el summ)) (cons x rst))))

(defun swirler-axial-area (d-sm d-big n-blades width-blades angle-blades)
 "@b(Описание:) swirler-axial-area выполняет расчет площади осевого завихрителя.

@b(Переменые:)
@begin(list)
 @item(d-sm         - меньший диаметр завихрителя;)
 @item(d-big        - больший диаметр завихрителя;)
 @item(n-blades     - количество лопаток завихрителя;)
 @item(width-blades - толщина лопаток завихрителя;)
 @item(angle-blades - угол закрутки потока лопатками, град;)
@end(list)
"
  (let ((a-b (* angle-blades (/ pi 180))))
    (- (* pi 0.25
	  (- (* d-big d-big) (* d-sm d-sm))
	  (cos a-b))
       (* n-blades width-blades 1/2 (- d-big d-sm)))))

(defun ring-area (d-big d-small)
  "@b(Описание:) функция @b(ring-area) возвращает площадь кольца.

 @b(Переменые:)
@begin(list)
 @item(d-big - наружный диаметр кольца;)
 @item(d-small - внутренний диаметр кольца.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (ring-area 20.0 10.0) => 235.61944901923448d0
@end(code)
"
  (- (circle-area-by-diameter d-big)
     (circle-area-by-diameter d-small)))

(defun ring-volume (d-big d-small hight)
  "@b(Описание:) функция @b(ring-volume) возвращает объем кольца.

 @b(Переменые:)
@begin(list)
 @item(d-big - наружный диаметр кольца;)
 @item(d-small - внутренний диаметр кольца;)
 @item(hight - высота.)
@end(list)
"
  (* (ring-area d-big d-small) hight))

(defun ring-mass (d-big d-small hight &optional (density (* 0.001 0.001 7.8)))
  "@b(Описание:) функция @b(ring-mass) возвращает массу кольца.

 @b(Переменые:)
@begin(list)
 @item(d-big - наружный диаметр кольца;)
 @item(d-small - внутренний диаметр кольца;)
 @item(hight - высота;)
 @item(density - плотность.)
@end(list)"
  (* (ring-volume d-big d-small hight) density))

(defun pipe-area (d-big wall)
  "@b(Описание:) функция @b(pipe-area) возвращает площадь трубы по
   наружному диаметру и толщине стенки.

 @b(Переменые:)
@begin(list)
 @item(d-big - наружный диаметр кольца;)
 @item(wall - внутренний диаметр кольца.)
@end(list)
"
  (- (circle-area-by-diameter d-big)
     (circle-area-by-diameter (- d-big wall wall))))

(defun pipe-volume (d-big wall length)
  "@b(Описание:) функция @b(pipe-volume) возвращает объем трубы по
   наружному диаметру, толщине стенки и длине."
  (* (pipe-area d-big wall) length))

(defun pipe-mass (d-big wall hight &optional (density (* 0.001 0.001 7.8)))
  "@b(Описание:) функция @b(pipe-mass) возвращает массу трубы.
 по наружному диаметру, толщине стенки, длине и плотности материала."
  (* (pipe-volume d-big wall hight) density))

(defun round-bar-mass (d-big length &optional (density (* 0.001 0.001 7.8)))
  "@b(Описание:) функция @b(round-bar-mass) возвращает массу кругляка.

 @b(Переменые:)
@begin(list)
 @item(d-big - наружный диаметр кругляка;)
 @item(length - длина;)
 @item(density - плотность.)
@end(list)"
  (* (circle-area-by-diameter d-big) length density))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ring-equal-area-radius (radius-big radius-small i n)
  "@b(Описание:) ring-equal-area-radius делит кольцо на n 
концентричных равновеликих по площади колец и возврвщает i-товый радиус.

@begin(list)
 @item(при i=0 возврвшается внутренний радиус кольца;)
 @item(при i=n возвращается наружный радиус кольца;)
@end(list)

@b(Переменые:)
@begin(list)
 @item(radius-big - наружный радиус кольца;)
 @item(radius-small - внутренний радиус кольца;)
 @item(i  - i-товый радиус;)
 @item(n  - количество концентричных колец.)
@end(list)

@b(Пример использования:)
@begin[lang=lisp](code)
 (ring-equal-area-radius 472.0 411.0 0 5) => 411.0
 (ring-equal-area-radius 472.0 411.0 1 5) => 423.9028
 (ring-equal-area-radius 472.0 411.0 5 5) => 472.0
@end(code)
"
  (sqrt (+ (* (- (* radius-big radius-big) (* radius-small radius-small)) i (/ 1.0 n))
	   (* radius-small radius-small))))

(defun ring-equal-area-border-radius-list (radius-big radius-small n )
  "@b(Описание:) ring-equal-area-radius-list возвращает радиусы границ 
 равновеликих площадей при делении кольца на n частей.

 @b(Переменые:)
@begin(list)
 @item(radius-big - Максимальный радиус;)
 @item(radius-small - Минимальный радиус;)
 @item(n  - Количество концентричных колец.)
@end(list)
"
  (loop :for i :from 0 :to n
        :collect (ring-equal-area-radius radius-big radius-small i n)))

(defun ring-equal-area-radius-list (radius-big radius-small n )
  "@b(Описание:) ring-equal-area-radius-list возвращает радиусы
центров масс равновеликих площадей при делении кольца на n частей.

 @b(Переменые:)
@begin(list)
 @item(radius-big - Максимальный радиус;)
 @item(radius-small - Минимальный радиус;)
 @item(n  - Количество концентричных колец.)
@end(list)
"
  (let ((m (* 2 n)))
    (loop :for i :from 1 :to m :by 2
	  :collect (ring-equal-area-radius radius-big radius-small i m))))

(defun ring-equal-area-radius-relative-higth-list (radius-big radius-small n)
  "@b(Описание:) ring-equal-area-radius-relative-higth-list возвращает
   относительные высоты центров масс равновеликих площадей при делении
   кольца на @b(n) частей. Относительная высота - расстояние от
   внутреннего радиуса до точки отнесенное к разности
   радиусов (наружный минус внутренний).

 @b(Переменые:)
@begin(list)
 @item(radius-big - больший радиус кольца;)
 @item(radius-small - меньший радиус кольца;)
 @item(n - количество полясов.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (ring-equal-area-radius-relative-higth-list 472.0 411.0 5)
 => (0.10657789 0.3149039 0.51724994 0.7141038 0.9058898)
@end(code)
"
  (let ((h (- radius-big radius-small)))
    (mapcar
     #'(lambda (el)
	 (/ (- el radius-small) h))
     (ring-equal-area-radius-list radius-big radius-small n))))

(defun hydraulic-diameter (area perimeter)
  "@b(Описание:) функция @b(hydraulic-diameter) возвращает
 гидравлический диаметр.

 @b(Пример использования:) @begin[lang=lisp](code)
 (hydraulic-diameter  (/ pi 4) pi) => 1.0d0 
@end(code)
"
  (/ (* 4 area) perimeter))

(defun equivalent-diameter (area)
  "@b(Описание:) функция @b(equivalent-diameter) возвращает
   эквивалентнный диаметр. Диаметр круга с равной площадью.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (equivalent-diameter (/ pi 4)) => 1.0d0 
@end(code)
"
  (circle-diameter-by-area area))

(defun simpson (a b func)
  "Формула Ньютона-Симпсона интегрирования на отрезке (a, b)."
  (* 1/6
     (- b a)
     (+ (funcall func a)
        (* 4
           (funcall func (* 1/2 (+ a b))))
        (funcall func b))))

(defun hydraulic-diameter-rect (a b)
           (areas:hydraulic-diameter (* a b) (+ a a b b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; defclass

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

(defun make-<circle> (diameter &key (center (make-instance '<point-3d>)))
  (make-instance '<circle> :radius (/ diameter 2) :center center))

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

