;;;; ucircom.asd

(asdf:defsystem #:ucircom
  :description "Describe ucircom here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:str #:serapeum)
  :components ((:file "package")
               (:file "ucircom")))
