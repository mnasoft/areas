;;;; methods.lisp

(in-package :areas)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; area ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod area ((x <circle>))
  "@b(Описание:) area возвращает плошадь круга.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (let ((c (make-instance '<circle>))) (print (list c (area c))))
@end(code)
"
  (* pi (<circle>-radius x) (<circle>-radius x)))

(defmethod area ((x <rectangle>))
"@b(Описание:) area возвращает плошадь прямоугольника.

 @b(Пример использования:) 
@begin[lang=lisp](code)
 (let ((c (make-instance 'rectangle))) (print (list c (area c))))
@end(code)
"
  (* (rectangle-length-1 x) (rectangle-length-2 x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; (setf area) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf area) (area (obj <circle>))
  ""
  (setf (circle-radius obj) (sqrt (/ area pi))))

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

(defmethod perimeter ((obj <circle>))
  ""
  (* 2 pi (circle-radius obj)))

(defmethod (setf perimeter) (perimeter (obj <circle>))
  ""
  (setf (circle-radius obj) (/ perimeter 2 pi)))

;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod perimeter ((obj <rectangle>))
  ""
  (* 2 (+  (rectangle-length-1 obj)
	   (rectangle-length-2 obj))))

(defmethod (setf perimeter) (perimeter (obj <rectangle>))
  ""
  (let ((p-scale (/ perimeter (perimeter obj))))
    (setf (rectangle-length-1 obj) (* p-scale (rectangle-length-1 obj))
	  (rectangle-length-2 obj) (* p-scale (rectangle-length-2 obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod radius ((circle <circle>))
  "@b(Описание:) метод @b(radius) возврвщает радиус круга @b(circle).

 @b(Пример использования:)
@begin[lang=lisp](code)
  (radius (make-instance '<circle>)) => 100.0
  (radius (make-instance '<circle> :radius 10.25)) => 10.25
@end(code)
"
  (<circle>-radius circle))

(defmethod diameter ((circle <circle>))
  "@b(Описание:) метод @b(diameter) возврвщает диаметр круга @b(circle).

 @b(Пример использования:)
@begin[lang=lisp](code)
  (radius (make-instance '<circle>)) => 100.0
  (radius (make-instance '<circle> :radius 10.25)) => 10.25
@end(code)
"
  (* 2 (<circle>-radius circle)))

(defmethod (setf radius)  (radius (circle <circle>))
  "@b(Описание:) setf-метод @b(setf radius) устанавливает радиус круга
  @b(circle) равным @b(radius).

 @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((circle (make-instance '<circle>)))
    (setf (radius circle) 51.25)
    (format t \"~A~%\" circle))
  -> #<circle>(r=51.25 c=#<point-3d>(0.0 0.0 0.0))
@end(code)
"
  (setf (<circle>-radius circle) radius))

(defmethod (setf diameter) (diameter (circle <circle>))
  "@b(Описание:) setf-метод @b(setf radius) устанавливает диаметр круга
  @b(circle) равным @b(diameter).

 @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((circle (make-instance '<circle>)))
    (setf (diameter circle) 51.25)
    (format t \"~A~%\" circle)) ->  #<circle>(r=25.625 c=#<point-3d>(0.0 0.0 0.0))
@end(code)
"
  (setf (<circle>-radius circle) (/ diameter 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric dxf-out (p &optional s)
  (:documentation "Выводит объект в dxf"))

(defmethod dxf-out ((p <point-3d>) &optional (s t))
"Пример использования:
;;;; (dxf-out (make-instance 'point-3d) nil)"
  (format s "0~%POINT~%10~%~A~%20~%~A~%30~%~A" (x p) (y p) (z p)))
