(cl:in-package #:asdf-user)

(defsystem #:cyclosis
  :serial t
  :components
  ((:file "packages")
   (:file "gray-streams")
   (:file "standard-streams")
   (:file "stream")))
