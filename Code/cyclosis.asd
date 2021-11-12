(cl:in-package #:asdf-user)

(defsystem #:cyclosis
  :depends-on (#:alexandria)
  :serial t
  :components
  ((:file "packages")
   (:file "gray-streams")
   (:file "standard-streams")
   (:file "stream")))
