(cl:in-package #:asdf-user)

(defsystem "cyclosis-extrinsic"
  :depends-on ("alexandria"
               "cyclosis")
  :in-order-to ((test-op (test-op "cyclosis-extrinsic/test")))
  :components ((:module "code"
                :pathname "code/extrinsic/"
                :serial t
                :components ((:file "packages")
                             (:file "interface")))))

(defsystem "cyclosis-extrinsic/test"
  :description "ANSI Test system for Cyclosis"
  :license "BSD"
  :author "Tarn W. Burton"
  :depends-on ("ansi-test-harness"
               "cyclosis-extrinsic")
  :perform (test-op (op c)
             (symbol-call :cyclosis-extrinsic/test :test))
  :components ((:module "code"
                :pathname "code/extrinsic/test/"
                :serial t
                :components ((:file "packages")
                             (:file "test")))))
