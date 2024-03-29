;;;; areas.asd

(defsystem "areas"
  :description "Describe areas here"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :depends-on ("convertion" "math/appr") ;;;; #:lst-arr
  :components ((:module "src"
		:serial nil
                :components ((:file "areas")
	                     (:file "methods" :depends-on ("areas"))))
;;;;	       (:file "test")
	       ))

(defsystem "areas/docs"
  :description "Зависимости для сборки документации"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("areas"
               "mnas-package"
               "codex"
               ))
