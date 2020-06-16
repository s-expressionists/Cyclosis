(cl:in-package #:asdf-user)

(defsystem #:cyclosis
  :serial t
  :components
  ((:file "packages")
   (:file "stream")
   (:file "standard-streams")))
