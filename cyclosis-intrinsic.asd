(cl:in-package #:asdf-user)

(defsystem "cyclosis-intrinsic"
  :depends-on ("alexandria"
               "cyclosis")
  :components ((:module "code"
                :pathname "code/intrinsic/"
                :serial t
                :components ((:file "packages")
                             (:file "interface")))))
