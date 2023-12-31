(cl:in-package #:asdf-user)

(defsystem "cyclosis-extrinsic"
  :depends-on ("alexandria"
               "cyclosis")
  :components ((:module "code"
                :pathname "code/extrinsic/"
                :serial t
                :components ((:file "packages")
                             (:file "interface")))))
