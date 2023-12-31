(cl:in-package #:asdf-user)

(defsystem "cyclosis"
  :depends-on ("alexandria")
  :components ((:module "code"
                :serial t
                :components ((:file "packages")
                             (:file "gray-streams")
                             (:file "standard-streams")
                             (:file "stream")))))
