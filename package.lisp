;;;; package.lisp

(defpackage #:areas)

(defpackage #:areas
  (:use #:cl #:convertion)
  (:export areas::circle
	   areas::circle-radius
	   areas::circle-center
	   )
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
  (:export areas::ring-equal-area-radius
	   areas::ring-equal-area-radius-list
	   areas::ring-equal-area-radius-relative-higth-list
	   )
  )
