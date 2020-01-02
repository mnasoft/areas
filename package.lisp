;;;; package.lisp

(defpackage #:areas)

(defpackage #:areas
  (:use #:cl #:cl-annot #:convertion)
  (:export areas::area
	   areas::perimeter)
  (:export areas::ring-area
	   areas::ring-volume ring-mass
	   )
  (:export areas::pipe-area
	   areas::pipe-volume pipe-mass
	   )
  (:export areas::circle-area-by-diameter
	   areas::circle-area-by-radius
	   areas::circle-diameter-by-area
	   areas::equivalent-area-group-holes
	   areas::parts
	   areas::axial-swirler
	   areas::pipe-mass
	   areas::round-bar-mass
	   )
  )

;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))
