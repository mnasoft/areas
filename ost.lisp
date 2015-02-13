
;;Выполняет вычисление неуказанного предельного отклонения согласно ОСТ 1 00022-80
;;Согласно п. 1.1
;;Параметры - x - номинальное значение размера
;;dim-type - строка принимающая, одно из значений
;;для отверстий: 
;;d - диаметр круглого отверстия;
;;b - ширина шпоночного паза, размер под ключ ;
;;l - длина шпоночного паза,  ;
;;d1 - расстояние по диагонали шестигранника, диаметр конической проточки;
;;d d2 b d1 l b1 d3 l1

(defun fields-val-lst(x)
  (cond
    ((< x 0.1) '(0.0 0.0 0.0 0.0))
    ((<= x 0.3) '(0.06 -0.06 0.1 -0.06))
    ((<= x 0.5) '(0.1 -0.1 0.14 -0.14))
    ((< x 1.0) '(0.1 -0.1 0.14 -0.14))
    ((<= x 3.0) '(0.14 -0.14 0.25 -0.25))
    ((<= x 6.0) '(0.22 -0.22 0.36 -0.36))
    ((<= x 18.0) '(0.27 -0.27 0.43 -0.43))
    ((<= x 30.0) '(0.33 -0.33 0.52 -0.52))
    ((<= x 50.0) '(0.39 -0.39 0.62 -0.62))
    ((<= x 80.0) '(0.46 -0.46 0.74 -0.74))
    ((<= x 120.0) '(0.54 -0.54 0.87 -0.87))
    ((<= x 180.0) '(0.63 -0.63 1.0 -1.0))
    ((<= x 250.0) '(0.72 -0.72 1.15 -1.15))
    ((<= x 315.0) '(0.81 -0.81 1.3 -1.3))
    ((<= x 400.0) '(0.89 -0.89 1.4 -1.4))
    ((<= x 500.0) '(0.97 -0.97 1.55 -1.55))
    ((<= x 630.0) '(1.1 -1.1 1.75 -1.75))
    ((<= x 800.0) '(1.25 -1.25 2.0 -2.0))
    ((<= x 1000.0) '(1.25 -1.25 2.3 -2.3))
    ((<= x 1250.0) '(1.25 -1.25 2.6 -2.6))
    ((<= x 1600.0) '(1.25 -1.25 3.1 -3.1))
    ((<= x 2000.0) '(1.5 -1.5 3.7 -3.7))
    ((<= x 2500.0) '(1.75 -1.75 4.4 -4.4))
    ((<= x 3150.0) '(2.1 -2.1 5.4 -5.4))
    ((<= x 4000.0) '(2.6 -2.6 6.6 -6.6))
    ((<= x 5000.0) '(3.2 -3.2 8.0 -8.0))
    ((<= x 6300.0) '(4.0 -4.0 9.8 -9.8))
    ((<= x 8000.0) '(4.9 -4.9 12.0 -12.0))
    ((<= x 10000.0) '(6.0 -6.0 15.0 -15.0))
    (T '(0.0 0.0 0.0 0.0))))

(defun fields-notval-lst(x)
  (cond
    ((< x 0.1) '(0.0 ))
    ((<= x 0.3) '(0.05))
    ((<= x 0.5) '(0.07))
    ((<= x 3.0) '(0.15))
    ((<= x 6.0) '(0.2))
    ((<= x 30.0) '(0.2))
    ((<= x 120.0) '(0.3))
    ((<= x 315.0) '(0.5))
    ((<= x 1000.0) '(0.8))
    ((<= x 2000.0) '(1.2))
    ((<= x 3150.0) '(2.0))
    ((<= x 5000.0) '(3.0))
    ((<= x 8000.0) '(5.0))
    ((<= x 10000.0) '(8.0))
    (T '(0.0))))

(defun fields-rezba-hole-lst(x)
  "l3 l4"
  (cond
    ((< x 0.1) '(0.0 0.0 0.0 0.0))
    ((<= x 0.3) '(0.0 0.0 -0.05 0.1))
    ((<= x 0.5) '(0.0 0.0 -0.1 0.2))
    ((<= x 3.0) '(-0.25 0.5 -0.25 0.5))
    ((<= x 6.0) '(-0.25 0.5 -0.25 0.5))
    ((<= x 315.0) '(-0.5 1.0 -0.5 1.0))
    (T '(0.0))))

(defun fields-chamfer-radius-lst(x)
  "c r"
  (cond
    ((= x 0.1) '(0.07))
    ((< x 0.3) '(0.1))
    ((<= x 0.5) '(0.2))
    ((<= x 3.0) '(0.3))
    ((<= x 6.0) '(0.5))
    ((<= x 30.0) '(1.0))
    ((<= x 120.0) '(2.0))
    ((<= x 1000.0) '(4.0))
    (T '(0.0))))

(defun map-or(str lst)
  (let ((rez nil))
    (mapc #'(lambda (el)
	      (if (equal el str) (setf rez (or rez t)))) lst) rez))

(defun field-val(x &optional (dim-type "d"))
 
  (cond
    ((equal dim-type "d") (values x (+ x (first (fields-val-lst x)))))
    ((equal dim-type "d2") (values x (+ x (second (fields-val-lst x)))))
    ((or (equal dim-type "b")
	 (equal dim-type "d1")
	 (equal dim-type "l"))
     (values x (+ x (third (fields-val-lst x)))))
    ((or (equal dim-type "b1")
	 (equal dim-type "d3")
	 (equal dim-type "l1"))
     (values x (+ x (fourth (fields-val-lst x)))))
    (T (values x x))))

(defun field-notval(x &optional (dim-type "A"))
  "A h l2 r0"
  (let ((field (first (fields-notval-lst x))))
    (cond
      ((or (equal dim-type "A")
	   (equal dim-type "h")
	   (equal dim-type "l2")
	   (equal dim-type "r0"))
       (values (- x field) (+ x field))))))

(defun fields-rezba-hole (x &optional (dim-type "l3"))
  "l3 l4"
  (cond
    ((equal dim-type "l3")
     (values (+ x (first(fields-rezba-hole-lst x)))
	     (+ x (second(fields-rezba-hole-lst x)))))
    ((equal dim-type "l4")
     (values (+ x (third(fields-rezba-hole-lst x)))
	     (+ x (fourth(fields-rezba-hole-lst x)))))))

(defun fields-chamfer-radius (x &optional (dim-type "c"))
  "c r"
    (let ((field (first (fields-chamfer-radius-lst x))))
    (cond
      ((map-or dim-type '("c" "e"))
       (values (- x field) (+ x field)))
      (T (values x x )))))

(defun field (x dim-type)
  (cond
    ((map-or dim-type '("d" "d2" "b" "d1" "l" "b1" "d3" "l1"))
     (field-val x dim-type))
    ((map-or dim-type '("A" "h" "l2" "r0"))
     (field-notval x dim-type))
    ((map-or dim-type '("l3" "l4"))
     (fields-rezba-hole x dim-type))
    ((map-or dim-type '("c" "r"))
     (fields-chamfer-radius x dim-type))))
