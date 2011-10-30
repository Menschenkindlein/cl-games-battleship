(defsystem cl-games-battleship
    :depends-on ()
    :components
    ((:module "src"
              :components
              ((:file "package")
	       (:module "utils" :depends-on ("package")
                        :components
                        ((:file "utils")
                         (:file "decart" :depends-on ("utils"))))
	       (:file "engine" :depends-on ("utils"))
	       (:module "bots" :depends-on ("engine")
			:components
			((:file "placers")
			 (:file "killers")))
	       (:module "interface" :depends-on ("engine" "bots")
			:components
			((:file "printer")
			 (:file "printer-console"
				:depends-on ("printer"))
			 (:file "interface"
				:depends-on ("printer-console"))))))))