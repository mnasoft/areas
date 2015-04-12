;;;; areas.asd

(asdf:defsystem #:areas
  :description "Describe areas here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:lst-arr #:dims)
  :components ((:file "package")
               (:file "areas")))

