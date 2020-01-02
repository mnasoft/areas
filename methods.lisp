;;;; methods.lisp

(in-package :areas)

(annot:enable-annot-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; area ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
@annot.doc:doc
"@b(Описание:) area возвращает плошадь круга.
@b(Пример использования:)
@begin[lang=lisp](code)
 (let ((c (make-instance '<circle>))) (print (list c (area c))))
@end(code)
"
(defmethod area ((x <circle>))
  (* pi (circle-radius x) (circle-radius x)))

@export
@annot.doc:doc
"@b(Описание:) area возвращает плошадь прямоугольника.
@b(Пример использования:)
@begin[lang=lisp](code)
(let ((c (make-instance 'rectangle))) (print (list c (area c))))
@end(code)
"
(defmethod area ((x <rectangle>))
  (* (rectangle-length-1 x) (rectangle-length-2 x)))

@export
@annot.doc:doc
"Вычисляет площадь осевого завихрителя
Пример использования:
;;;; (let ((aaa (make-instance 'zavihritel-osevoy))) (print  (list aaa (area aaa))))
"
(defmethod area ((x <zavihritel-osevoy>))
  (- (* pi 1/4 (cos (degrees->radians (vane-angle x)))
	(- (* (out-diameter x) (out-diameter x))
	   (* (in-diameter x) (in-diameter x))))
     (* 1/2 (vane-number x) (vane-width x) (- (out-diameter x) (in-diameter x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; (setf area) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
@annot.doc:doc
""
(defmethod (setf area) (area (obj <circle>))
  (setf (circle-radius obj) (sqrt (/ area pi))))

@export
@annot.doc:doc
"@b(Пример использования:)
@begin[lang=lisp](code)
(let ((r (make-instance 'rectangle :length-1 2 :length-2 3)))
  (format t \"~a~%\" r)
  (setf (area r) 12)
  (format t \"~a~%\" r))
@end(code)
"
(defmethod (setf area) (area (obj <rectangle>))
  (let ((area-scale (sqrt (/ area (area obj)))))
    (setf (rectangle-length-1 obj) (* (rectangle-length-1 obj) area-scale))
    (setf (rectangle-length-2 obj) (* (rectangle-length-2 obj) area-scale))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
@annot.doc:doc
""
(defmethod perimeter ((obj <circle>))
  (* 2 pi (circle-radius obj)))

@export
@annot.doc:doc
""
(defmethod (setf perimeter) (perimeter (obj <circle>))
  (setf (circle-radius obj) (/ perimeter 2 pi)))

;;;;;;;;;;;;;;;;;;;;;;;;;

@export
@annot.doc:doc
""
(defmethod perimeter ((obj <rectangle>))
  (* 2 (+  (rectangle-length-1 obj)
	   (rectangle-length-2 obj))))

@export
@annot.doc:doc
""
(defmethod (setf perimeter) (perimeter (obj <rectangle>))
  (let ((p-scale (/ perimeter (perimeter obj))))
    (setf (rectangle-length-1 obj) (* p-scale (rectangle-length-1 obj))
	  (rectangle-length-2 obj) (* p-scale (rectangle-length-2 obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
@annot.doc:doc
""
(defmethod  radius ((obj <circle>))
  (circle-radius obj))

@export
@annot.doc:doc
""
(defmethod diameter ((obj <circle>))
  (* 2 (circle-radius obj)))

@export
@annot.doc:doc
""
(defmethod (setf radius) (radius obj)
  (setf (circle-radius obj) radius))

@export
@annot.doc:doc
""
(defmethod (setf diameter) (diameter (obj <circle>))
  (setf (circle-radius obj) (/ diameter 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
@annot.doc:doc
""
(defgeneric dxf-out (p &optional s) (:documentation "Выводит объект в dxf"))

@export
@annot.doc:doc
"Пример использования:
;;;; (dxf-out (make-instance 'point-3d) nil)"
(defmethod dxf-out ((p <point-3d>) &optional (s t))

    (format s "0~%POINT~%10~%~A~%20~%~A~%30~%~A" (x p) (y p) (z p)))
