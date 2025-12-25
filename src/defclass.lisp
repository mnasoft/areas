;;;; package.lisp

(in-package :areas)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <ring> (<areable> <perimetrable>)
  ((diameter-big   :accessor <ring>-diameter-big
                   :initarg :diameter-big
                   :initform 100.0
                   :documentation "Радиус окружности.")
   (diameter-small :accessor <ring>-diameter-small
                   :initarg :diameter-small
                   :initform 50.0
                   :documentation "Радиус окружности.")
   (center
    :accessor <circle>-center
    :initarg :center
    :initform (make-instance '<point-3d>)
    :documentation "Радиус окружности."))
  (:documentation "@b(Описание:) класс @b(<circle>) представляет круг."))

(math/coord:dtr 30.0)

(* 1/60
   (cos (math/coord:dtr 30.0))
   (-
    (area
     (make-instance '<ring> :diameter-big 60.0 :diameter-small 55.705))
    264.345))  ; => 1.8180874352712035d0 (181.80874352712036d0%)


(defmethod area ((obj <ring>))
  (let ((db (<ring>-diameter-big obj))
        (ds (<ring>-diameter-small obj)))
    (* (- db ds) (+ db ds) pi 1/4
    )))
