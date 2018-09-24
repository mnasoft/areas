;;;; package.lisp

(defpackage #:areas
  (:use #:cl #:convertion)
  (:export circle
	   circle-radius
	   circle-center)
  (:export area
	   perimeter)
  (:export ring-area ring-volume ring-mass)
  (:export pipe-area pipe-volume pipe-mass)
  )
