(defsystem war
    :depends-on (#:cl)
    :components
    ((:module "src"
              :components
              ((:file "engine")
               (:file "placers" :depends-on ("engine"))
               (:file "killers" :depends-on ("engine"))
               (:file "interface" :depends-on ("engine"
					       "placers"
					       "killers"))))))