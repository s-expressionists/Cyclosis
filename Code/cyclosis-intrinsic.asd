(cl:in-package #:asdf-user)

(defsystem #:cyclosis-intrinsic
  :depends-on (#:alexandria)
  :serial t
  :components
  ((:file "packages-intrinsic")
   (:file "gray-streams")
   (:file "standard-streams")
   (:file "stream")))
