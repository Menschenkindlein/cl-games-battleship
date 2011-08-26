(defsystem cl-games-battleship
    :depends-on ()
    :components
    ((:module "src"
              :components
              ((:file "package")
	       (:file "engine" :depends-on ("package"))
               (:file "placers" :depends-on ("engine"))
               (:file "killers" :depends-on ("engine"))
               (:file "printer" :depends-on ("engine"))
               (:file "interface" :depends-on ("engine"
					       "placers"
					       "killers"
					       "printer"))))))