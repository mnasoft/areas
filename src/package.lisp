;;;; package.lisp

(defpackage #:areas
  (:use #:cl #:convertion)
  (:export circle-area-by-diameter
           circle-area-by-radius
           circle-diameter-by-area    
           parts
           ring-area
           ring-volume
           ring-mass
           pipe-area
           pipe-volume
           pipe-mass
           round-bar-mass
           )
  (:export equivalent-area-group-holes)
  (:export ring-equal-area-radius
           ring-equal-area-radius-list
           ring-equal-area-radius-relative-higth-list)
  (:export swirler-axial-area)
  (:export <swirler-axial>
           <swirler-axial>-out-diameter
           <swirler-axial>-in-diameter
           <swirler-axial>-vane-number
           <swirler-axial>-vane-angle
           <swirler-axial>-vane-width
           )
  (:export <point-3d>
           <point-3d>-x
           <point-3d>-y
           <point-3d>-z
           )
  (:export <circle>
           <circle>-radius
           <circle>-center
           )
  (:export <circular-segment>
           <circular-segment>-angle 
           heigth
           width
           )
  (:export <rectangle>
           <rectangle>-length-1
           <rectangle>-length-2
           <rectangle>-center
           <rectangle>-angle
           )
  (:export <romb>)
  (:export area
           radius
           diameter
           perimeter
           dxf-out)
  (:export hydraulic-diameter
           equivalent-diameter
           )
  (:export simpson)
  (:export <pnt-curve>
           <pnt-curve>-points
           y-by-x
           integrate))

;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))

