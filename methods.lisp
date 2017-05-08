;;;; methods.lisp

(in-package :areas)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric area (x)
  (:documentation "Вычисляет площадь"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod area ((x circle))
  "Возвращает плошадь круга 
Пример использования:
;;;; (let ((c (make-instance 'circle))) (print (list c (area c))))
"
  (* pi (circle-radius x) (circle-radius x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod area ((x rectangle))
  "Возвращает плошадь прямоугольника 
Пример использования:
;;;; (let ((c (make-instance 'rectangle))) (print (list c (area c))))
"
  (* (rectangle-length-1 x) (rectangle-length-2 x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod area ((x zavihritel-osevoy))
    "Вычисляет площадь осевого завихрителя
Пример использования:
;;;; (let ((aaa (make-instance 'zavihritel-osevoy))) (print  (list aaa (area aaa))))
"
  (- (* pi 1/4 (cos (degrees->radians (vane-angle x)))
	(- (* (out-diameter x) (out-diameter x))
	   (* (in-diameter x) (in-diameter x))))
     (* 1/2 (vane-number x) (vane-width x) (- (out-diameter x) (in-diameter x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric dxf-out (p &optional s) (:documentation "Выводит объект в dxf"))

(defmethod dxf-out ((p point-3d) &optional (s t))
  "Пример использования:
;;;; (dxf-out (make-instance 'point-3d) nil)"
    (format s "0~%POINT~%10~%~A~%20~%~A~%30~%~A" (x p) (y p) (z p)))


