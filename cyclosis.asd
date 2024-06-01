(cl:in-package #:asdf-user)

(defsystem "cyclosis"
  :depends-on ("alexandria")
  :components ((:module "code"
                :serial t
                :components ((:file "packages")
                             (:file "interface")
                             (:file "conditions")
                             (:file "default-methods")
                             (:file "stream")
                             (:file "broadcast-stream")
                             (:file "concatenated-stream")
                             (:file "echo-stream")
                             (:file "string-stream")
                             (:file "synonym-stream")
                             (:file "two-way-stream")
                             (:file "posix-file-stream")
                             (:module "transcode"
                              :serial t
                              :components ((:file "common")
                                           (:file "character")
                                           (:file "ascii")
                                           (:file "iso-8859-1")
                                           (:file "utf-8")
                                           (:file "utf-32")
                                           (:file "unsigned-byte")))))))
