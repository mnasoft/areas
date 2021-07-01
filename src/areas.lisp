;;;; areas.lisp

(in-package #:areas)

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

(defun axial-swirler (d-sm d-big n-blades width-blades angle-blades)
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
  "@b(Описание:) функция @b(hydraulic-diameter) возвращает гидравлический диаметр.

 @b(Пример использования:)
@begin[lang=lisp](code)
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
