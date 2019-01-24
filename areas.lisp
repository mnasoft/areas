;;;; areas.lisp

;;;;(declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:areas)

;;; "areas" goes here. Hacks and glory await!


(defun circle-area-by-diameter(d)
  "Вычисляет площадь отверстия по его диаметру"
  (* d d 0.25 pi))


(defun circle-area-by-radius(r)
  "Вычисляет площадь отверстия по его диаметру"
  (* r r pi))


(defun circle-diameter-by-area(a)
  "Вычисляет диаметр круга по его площади"
  (sqrt (/ (* 4 a) pi)))


(defun equivalent-area-group-holes (x &rest rst)
  "Вычисляет эквивалентную площадь последовательно расположенных отверстий"
  (/ (sqrt (apply #'+ (mapcar #'(lambda(el) (/ 1.0 el el)) (cons x rst))))))


(defun parts(x &rest rst)
  "Возвращает доли, задаваемые списком аргументов
Пример использования:
;;;; (parts 1.0 2.0 1.0)
;;;; =>(0.25 0.5 0.25)
"
  (let ((summ (apply #'+ (cons x rst))))
    (mapcar #'(lambda (el) (/ el summ)) (cons x rst))))


(defun axial-swirler (d-sm d-big n-blades width-blades angle-blades)
  "Выполняет расчет площади осевого завихрителя;
| d-sm         | - | меньший диаметр                      |
|--------------+---+--------------------------------------|
| d-big        | - | больший диаметр                      |
| n-blades     | - | количество лопаток завихрителя       |
| width-blades | - | толщина лопаток завихрителя          |
| angle-blades | - | угол какрутки потока лопатками, град |
"
  (let ((a-b (* angle-blades (/ pi 180))))
    (- (* pi 0.25
	  (- (* d-big d-big) (* d-sm d-sm))
	  (cos a-b))
       (* n-blades width-blades 1/2 (- d-big d-sm)))))

(defun ring-area (d-big d-small)
  (- (circle-area-by-diameter d-big)
     (circle-area-by-diameter d-small)))

(defun ring-volume (d-big d-small hight)
  (* (ring-area d-big d-small) hight))

(defun ring-mass (d-big d-small hight &optional (density (* 0.001 0.001 7.8)))
  (* (ring-volume d-big d-small hight) density))

(defun pipe-area (d-big wall)
  (- (circle-area-by-diameter d-big)
     (circle-area-by-diameter (- d-big wall wall))))

(defun pipe-volume (d-big wall hight)
  (* (pipe-area d-big wall) hight))

(defun pipe-mass (d-big wall hight &optional (density (* 0.001 0.001 7.8)))
  (* (pipe-volume d-big wall hight) density))

(defun round-bar-mass (d-big hight &optional (density (* 0.001 0.001 7.8)))
  (* (circle-area-by-diameter d-big)  hight density))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ring-equal-area-radius (rb rm i n)
  "Делит кольцо на n концентричных равновеликих по площади
 колец и возврвщает i-товый радиус.
При i=0 возврвшается внутренний радиус кольца;
При i=n возвращается наружный радиус кольца;
Параметры:
 rb - Максимальный радиус (наружный радиус кольца);
 rm - Минимальный радиус (внутренний радиус кольца);
 i  - i-товый радиус;
 n  - Количество концентричных колец.
 Пример использования:
 (equal-area-radius 472.0 411.0 1 5)
"
  (sqrt (+ (* (- (* rb rb) (* rm rm)) i (/ 1.0 n))
	   (* rm rm))))

(defun ring-equal-area-radius-list (rb rm n )
  "Возвращает радиусы центров масс равновеликих площадей 
при делении кольца на 
 Параметры:
 rb - Максимальный радиус;
 rm - Минимальный радиус;
 n  - Количество концентричных колец.
"
  (let ((m (* 2 n)))
    (loop :for i :from 1 :to m :by 2
	  :collect (ring-equal-area-radius rb rm i m))))

(defun ring-equal-area-radius-relative-higth-list (rb rm n)
  "Возвращает относительные высоты в центре масс 
равновеликих площадей при делении кольца на n частей"
  (let ((h (- rb rm)))
    (mapcar
     #'(lambda (el)
	 (/ (- el rm) h))
    (ring-equal-area-radius-list rb rm n))))
