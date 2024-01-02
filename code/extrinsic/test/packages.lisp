(in-package #:common-lisp-user)

(defpackage #:cyclosis-extrinsic/test
  (:use #:common-lisp)
  (:shadow #:format
           #:read)
  (:export #:test))
