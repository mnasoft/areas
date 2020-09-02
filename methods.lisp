;;;; methods.lisp

(in-package :areas)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; area ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'area )

(defmethod area ((x <circle>))
  "@b(Описание:) area возвращает плошадь круга.
@b(Пример использования:)
@begin[lang=lisp](code)
 (let ((c (make-instance '<circle>))) (print (list c (area c))))
@end(code)
"
  (* pi (circle-radius x) (circle-radius x)))

(export 'area )

(defmethod area ((x <rectangle>))
"@b(Описание:) area возвращает плошадь прямоугольника.
@b(Пример использования:)
@begin[lang=lisp](code)
(let ((c (make-instance 'rectangle))) (print (list c (area c))))
@end(code)
"
  (* (rectangle-length-1 x) (rectangle-length-2 x)))

(export 'area )

(defmethod area ((x <zavihritel-osevoy>))
"Вычисляет площадь осевого завихрителя
Пример использования:
;;;; (let ((aaa (make-instance 'zavihritel-osevoy))) (print  (list aaa (area aaa))))
"
  (- (* pi 1/4 (cos (degrees->radians (vane-angle x)))
	(- (* (out-diameter x) (out-diameter x))
	   (* (in-diameter x) (in-diameter x))))
     (* 1/2 (vane-number x) (vane-width x) (- (out-diameter x) (in-diameter x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; (setf area) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'area)

(defmethod (setf area) (area (obj <circle>))
  ""
  (setf (circle-radius obj) (sqrt (/ area pi))))

(export 'area)
  
(defmethod (setf area) (area (obj <rectangle>))
  "@b(Пример использования:)

@begin[lang=lisp](code)
 (let ((r (make-instance 'rectangle :length-1 2 :length-2 3)))
  (format t \"~a~%\" r)
  (setf (area r) 12)
  (format t \"~a~%\" r))
@end(code)
"
  (let ((area-scale (sqrt (/ area (area obj)))))
    (setf (rectangle-length-1 obj) (* (rectangle-length-1 obj) area-scale))
    (setf (rectangle-length-2 obj) (* (rectangle-length-2 obj) area-scale))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'perimeter )

(defmethod perimeter ((obj <circle>))
  ""
  (* 2 pi (circle-radius obj)))

(export 'perimeter)

(defmethod (setf perimeter) (perimeter (obj <circle>))
  ""
  (setf (circle-radius obj) (/ perimeter 2 pi)))

;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'perimeter )

(defmethod perimeter ((obj <rectangle>))
  ""
  (* 2 (+  (rectangle-length-1 obj)
	   (rectangle-length-2 obj))))

(export 'perimeter)

(defmethod (setf perimeter) (perimeter (obj <rectangle>))
  ""
  (let ((p-scale (/ perimeter (perimeter obj))))
    (setf (rectangle-length-1 obj) (* p-scale (rectangle-length-1 obj))
	  (rectangle-length-2 obj) (* p-scale (rectangle-length-2 obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'radius )

(defmethod  radius ((obj <circle>))
""
  (circle-radius obj))

(export 'diameter )

(defmethod diameter ((obj <circle>))
  ""
  (* 2 (circle-radius obj)))

(export 'radius)

(defmethod (setf radius)  (radius obj)
  ""
  (setf (circle-radius obj) radius))

(export 'diameter)

(defmethod (setf diameter)  (diameter (obj <circle>))
  ""
  (setf (circle-radius obj) (/ diameter 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'dxf-out )

(defgeneric dxf-out (p &optional s)
  (:documentation "Выводит объект в dxf"))

(export 'dxf-out )

(defmethod dxf-out ((p <point-3d>) &optional (s t))
"Пример использования:
;;;; (dxf-out (make-instance 'point-3d) nil)"
  (format s "0~%POINT~%10~%~A~%20~%~A~%30~%~A" (x p) (y p) (z p)))
