(cl:in-package #:asdf-user)

(defsystem "cyclosis"
  :depends-on ("alexandria")
  :components ((:module "code"
                :serial t
                :components ((:file "packages")
                             (:file "interface")
                             (:file "default-methods")
                             (:file "stream")
                             (:file "broadcast-stream")
                             (:file "concatenated-stream")
                             (:file "echo-stream")
                             (:file "string-stream")
                             (:file "synonym-stream")
                             (:file "two-way-stream")
                             (:file "transcoders")))))
