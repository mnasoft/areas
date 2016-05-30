;;;; areas.lisp

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




