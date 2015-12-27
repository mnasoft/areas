;;;; angle-conversion.lisp

(in-package #:angle-conversion)

(defun degrees-to-radians (d)
  "Преобразование градусов в радианы"
  (/ d (/ 180 pi)))

(defun radians-to-degrees (r)
  "Преобразование  радианы в градусов"
  (/ r (/ pi 180)))

(defun degrees-minutes-seconds-to-radians (d m s)
  "Преобразование  угловой меры, выраженной в градусах минутах и секундах в радианы"
  (/ (+ d (/ m 60) (/ s 60 60)) (/ 180 pi)))

(defun degrees-to-degrees-minutes(deg)
  "Преобразование  угловой меры, выраженной в градусах в градусы и  минуты"
  (multiple-value-bind (d m)
      (truncate deg)
    (list d (* m 60d0))))

(defun degrees-to-degrees-minutes-seconds(deg)
  "Преобразование  угловой меры, выраженной в градусах в градусы и  минуты и секунды"
  (let*
      (
       (d1 (degrees-to-degrees-minutes deg))
       (d  (first d1))
       (d2 (degrees-to-degrees-minutes (second d1 )))
       (m  (first d2))
       (s (second d2)))
    (list d m s)
    ))

(defun radians-to-degrees-minutes-seconds (r)
  "Преобразование  радианов в градусы минуты и секунды"
  (degrees-to-degrees-minutes (radians-to-degrees r)))
