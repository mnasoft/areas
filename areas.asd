;;;; areas.asd

(asdf:defsystem #:areas
  :description "Describe areas here"
  :author "Your Name <your.name@example.com>"
  :license "GNU General Public License version 3"
  :serial t
  :depends-on (#:lst-arr)
  :components ((:file "package")
               (:file "areas")
	       (:file "angle-conversion")
	       (:file "classes")
;;;;	       (:file "test")
	       ))
