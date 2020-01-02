;;;; methods.lisp

(in-package :areas)

(defgeneric area   (obj)
  (:documentation "@b(Описание:) area возвращает полщадь объекта."))

(defgeneric (setf area) (area obj)
  (:documentation "@b(Описание:) (setf area) устанавливает полщадь объекта."))

(defgeneric perimeter (obj)
  (:documentation "@b(Описание:) perimeter возвращает периметр объекта."))

(defgeneric (setf perimeter) (perimeter obj)
  (:documentation "@b(Описание:) (setf perimeter) устанавливает периметр объекта."))

(defgeneric radius (obj)
  (:documentation "@b(Описание:) radius возвращает радиус объекта."))

(defgeneric (setf radius) (radius obj)
  (:documentation "@b(Описание:) (setf radius) устанавливает радиус для объекта."))

(defgeneric diameter (obj)
  (:documentation "@b(Описание:) diameter возвращает диаметр объекта."))

(defgeneric (setf diameter) (diameter obj)
  (:documentation "@b(Описание:) radius устанавливает диаметр объекта."))


(defgeneric dxf-out (p &optional s) (:documentation "Выводит объект в dxf"))

