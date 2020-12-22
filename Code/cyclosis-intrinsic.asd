(cl:in-package #:asdf-user)

(defsystem #:cyclosis-intrinsic
  :serial t
  :components
  ((:file "packages-intrinsic")
   (:file "gray-streams")
   (:file "standard-streams")
   (:file "stream")))
