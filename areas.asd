;;;; areas.asd

(defsystem #:areas
  :description "Describe areas here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :depends-on (#:cl-annot #:lst-arr #:convertion)
  :components (
	       (:file "package")
               (:file "areas")
	       (:file "classes")
       	       (:file "generics" :depends-on ("classes"))
	       (:file "methods"  :depends-on ("classes" "generics"))
;;;;	       (:file "test")
	       ))
