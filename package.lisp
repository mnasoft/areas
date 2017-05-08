;;;; package.lisp

(defpackage #:areas
  (:use #:cl #:convertion)
  (:export
   circle-area-by-diameter
   circle-area-by-radius
   circle-diameter-by-area
   equivalent-area-group-holes
   parts
   axial-swirler
   )
  (:export circle
	   circle-radius
	   circle-center)
  (:export area
	   perimeter)
  )

;;;;(declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))


