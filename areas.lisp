;;;; areas.lisp

;;;;(declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:areas)

(annot:enable-annot-syntax)

@export
@annot.doc:doc
"@b(Описание:) circle-area-by-diameter вычисляет площадь круга по его диаметру."
(defun circle-area-by-diameter (d)
  (* d d 0.25 pi))

@export
@annot.doc:doc
"@b(Описание:) circle-area-by-radius вычисляет площадь круга по его диаметру."
(defun circle-area-by-radius (r)
  (* r r pi))

@export
@annot.doc:doc
"@b(Описание:) circle-diameter-by-area вычисляет диаметр круга по его площади."
(defun circle-diameter-by-area (a)
  (sqrt (/ (* 4 a) pi)))

@export
@annot.doc:doc
"Вычисляет эквивалентную площадь последовательно расположенных отверстий"
(defun equivalent-area-group-holes (x &rest rst)
  (/ (sqrt (apply #'+ (mapcar #'(lambda(el) (/ 1.0 el el)) (cons x rst))))))

@export
@annot.doc:doc
"Возвращает доли, задаваемые списком аргументов
Пример использования:
;;;; (parts 1.0 2.0 1.0)
;;;; =>(0.25 0.5 0.25)
"
(defun parts(x &rest rst)
  (let ((summ (apply #'+ (cons x rst))))
    (mapcar #'(lambda (el) (/ el summ)) (cons x rst))))

@export
@annot.doc:doc
"@b(Описание:) axial-swirler выполняет расчет площади осевого завихрителя.

@b(Переменые:)
@begin(list)
 @item(d-sm         - меньший диаметр завихрителя;)
 @item(d-big        - больший диаметр завихрителя;)
 @item(n-blades     - количество лопаток завихрителя;)
 @item(width-blades - толщина лопаток завихрителя;)
 @item(angle-blades - угол закрутки потока лопатками, град;)
@end(list)
"
(defun axial-swirler (d-sm d-big n-blades width-blades angle-blades)
  (let ((a-b (* angle-blades (/ pi 180))))
    (- (* pi 0.25
	  (- (* d-big d-big) (* d-sm d-sm))
	  (cos a-b))
       (* n-blades width-blades 1/2 (- d-big d-sm)))))

@export
@annot.doc:doc
"Площадь кольца"
(defun ring-area (d-big d-small)
  (- (circle-area-by-diameter d-big)
     (circle-area-by-diameter d-small)))

@export
@annot.doc:doc
"Объем кольца"
(defun ring-volume (d-big d-small hight)
  (* (ring-area d-big d-small) hight))

@export
@annot.doc:doc
"Объем кольца"
(defun ring-mass (d-big d-small hight &optional (density (* 0.001 0.001 7.8)))
  (* (ring-volume d-big d-small hight) density))

@export
@annot.doc:doc
"Площадь трубы по наружному диаметру и толщине стенки."
(defun pipe-area (d-big wall)
  (- (circle-area-by-diameter d-big)
     (circle-area-by-diameter (- d-big wall wall))))

@export
@annot.doc:doc
"Объем трубы по наружному диаметру, толщине стенки и длине."
(defun pipe-volume (d-big wall length)
  (* (pipe-area d-big wall) length))


@export
@annot.doc:doc
"Масса трубы по наружному диаметру, толщине стенки, длине и плотности материала."
(defun pipe-mass (d-big wall hight &optional (density (* 0.001 0.001 7.8)))
  (* (pipe-volume d-big wall hight) density))

@export
@annot.doc:doc
"Масса кругляка по наружному диаметру, длине и плотности материала."
(defun round-bar-mass (d-big length &optional (density (* 0.001 0.001 7.8)))
  (* (circle-area-by-diameter d-big) length density))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
@annot.doc:doc
"@b(Описание:) ring-equal-area-radius делит кольцо на n 
концентричных равновеликих по площади колец и возврвщает i-товый радиус.
@begin(list)
 @item(при i=0 возврвшается внутренний радиус кольца;)
 @item(при i=n возвращается наружный радиус кольца;)
@end(list)

@b(Переменые:)
@begin(list)
 @item(rb - Максимальный радиус (наружный радиус кольца);)
 @item(rm - Минимальный радиус (внутренний радиус кольца);)
 @item(i  - i-товый радиус;)
 @item(n  - Количество концентричных колец.)
@end(list)

@b(Пример использования:)
@begin[lang=lisp](code)
 (ring-equal-area-radius 472.0 411.0 0 5) => 411.0
 (ring-equal-area-radius 472.0 411.0 1 5) => 423.9028
 (ring-equal-area-radius 472.0 411.0 5 5) => 472.0
@end(code)
"
(defun ring-equal-area-radius (rb rm i n)
  (sqrt (+ (* (- (* rb rb) (* rm rm)) i (/ 1.0 n))
	   (* rm rm))))

@export
@annot.doc:doc
"@b(Описание:) ring-equal-area-radius-list возвращает радиусы
центров масс равновеликих площадей при делении кольца на n частей.
@b(Переменые:)
@begin(list)
 @item(rb - Максимальный радиус;)
 @item(rm - Минимальный радиус;)
 @item(n  - Количество концентричных колец.)
@end(list)
"
(defun ring-equal-area-radius-list (rb rm n )
  (let ((m (* 2 n)))
    (loop :for i :from 1 :to m :by 2
	  :collect (ring-equal-area-radius rb rm i m))))

@export
@annot.doc:doc
"@b(Описание:) ring-equal-area-radius-relative-higth-list
Возвращает относительные в центров масс равновеликих площадей 
при делении кольца на n частей."
(defun ring-equal-area-radius-relative-higth-list (rb rm n)
  (let ((h (- rb rm)))
    (mapcar
     #'(lambda (el)
	 (/ (- el rm) h))
    (ring-equal-area-radius-list rb rm n))))
