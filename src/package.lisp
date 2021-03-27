;;;; package.lisp

(defpackage #:areas
  (:use #:cl #:convertion)
  (:export circle-area-by-diameter
           circle-area-by-radius
           circle-diameter-by-area
           equivalent-area-group-holes
           parts
           axial-swirler
           ring-area
           ring-volume
           ring-mass
           pipe-area
           pipe-volume
           pipe-mass
           round-bar-mass
           ring-equal-area-radius
           ring-equal-area-radius-list
           ring-equal-area-radius-relative-higth-list
           )
  (:export <zavihritel-osevoy>
           <zavihritel-osevoy>-out-diameter
           <zavihritel-osevoy>-in-diameter
           <zavihritel-osevoy>-vane-number
           <zavihritel-osevoy>-vane-angle
           <zavihritel-osevoy>-vane-width
           )
  (:export <point-3d>
           <point-3d>-x
           <point-3d>-y
           <point-3d>-z
           )
  (:export <circle>
           <circle>-radius
           <circle>-center)
  (:export <rectangle>
           <rectangle>-length-1
           <rectangle>-length-2
           <rectangle>-center
           <rectangle>-angle
           )
  (:export <romb>))


;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))
