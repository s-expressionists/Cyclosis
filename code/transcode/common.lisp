(cl:in-package #:cyclosis)

(defclass transcoder ()
  ((replacement :reader replacement
                :initform nil
                :initarg :replacement)))

(defun illegal-sequence (transcoder stream &rest octets)
  (or (replacement transcoder)
      (error 'illegal-sequence :stream stream :octets octets)))

(defun unexpected-eof (transcoder stream &rest octets)
  (or (replacement transcoder)
      (error 'unexpected-eof :stream stream :octets octets)))

(defun invalid-element (transcoder stream element)
  (or (replacement transcoder)
      (error 'invalid-element :stream stream :element element)))
