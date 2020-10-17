;;;; methods.lisp

(in-package :areas)

(export 'area   )

(defgeneric area   (obj)
  (:documentation "@b(Описание:) area возвращает полщадь объекта."))

(export 'area)

(defgeneric (setf area) (new-area obj)
  (:documentation "@b(Описание:) (setf area) устанавливает полщадь объекта."))

(export 'perimeter )

(defgeneric perimeter (obj)
  (:documentation "@b(Описание:) perimeter возвращает периметр объекта."))

(export 'perimeter)

(defgeneric (setf perimeter) (new-perimeter obj)
  (:documentation "@b(Описание:) (setf perimeter) устанавливает периметр объекта."))

(export 'radius )

(defgeneric radius (obj)
  (:documentation "@b(Описание:) radius возвращает радиус объекта."))

(export 'radius)

(defgeneric (setf radius) (new-radius obj)
  (:documentation "@b(Описание:) (setf radius) устанавливает радиус для объекта."))

(export 'diameter )

(defgeneric diameter (obj)
  (:documentation "@b(Описание:) diameter возвращает диаметр объекта."))

(export 'diameter )

(defgeneric (setf diameter) (new-diameter obj)
  (:documentation "@b(Описание:) radius устанавливает диаметр объекта."))

(export 'dxf-out )

(defgeneric dxf-out (p &optional s) (:documentation "Выводит объект в dxf"))
