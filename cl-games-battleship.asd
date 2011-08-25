(defsystem cl-games-battleship
    :depends-on ()
    :components
    ((:module "src"
              :components
              ((:file "package")
	       (:file "engine" :depends-on ("package"))
               (:file "placers" :depends-on ("engine"))
               (:file "killers" :depends-on ("package"
					     "engine"))
               (:file "interface" :depends-on ("package"
					       "engine"
					       "placers"
					       "killers"))))))