;;;; methods.lisp

(in-package :areas)

(annot:enable-annot-syntax)

@export
(defgeneric area   (obj)
  (:documentation "@b(Описание:) area возвращает полщадь объекта."))

@export
(defgeneric (setf area) (new-area obj)
  (:documentation "@b(Описание:) (setf area) устанавливает полщадь объекта."))

@export
(defgeneric perimeter (obj)
  (:documentation "@b(Описание:) perimeter возвращает периметр объекта."))

@export
(defgeneric (setf perimeter) (new-perimeter obj)
  (:documentation "@b(Описание:) (setf perimeter) устанавливает периметр объекта."))

@export
(defgeneric radius (obj)
  (:documentation "@b(Описание:) radius возвращает радиус объекта."))

@export
(defgeneric (setf radius) (new-radius obj)
  (:documentation "@b(Описание:) (setf radius) устанавливает радиус для объекта."))

@export
(defgeneric diameter (obj)
  (:documentation "@b(Описание:) diameter возвращает диаметр объекта."))

@export
(defgeneric (setf diameter) (new-diameter obj)
  (:documentation "@b(Описание:) radius устанавливает диаметр объекта."))

@export
(defgeneric dxf-out (p &optional s) (:documentation "Выводит объект в dxf"))
