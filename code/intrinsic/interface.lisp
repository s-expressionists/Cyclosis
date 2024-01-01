(cl:in-package #:cyclosis-intrinsic)

(defclass intrinsic-client ()
  ())

(defparameter *client* (make-instance 'intrinsic-client))
