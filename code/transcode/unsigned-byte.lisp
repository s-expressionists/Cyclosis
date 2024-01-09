(cl:in-package #:cyclosis)

;;; TODO: Implement signed-byte transcoding and improve unsigned-byte
;;; trancoding. Currenntly, these transcoders are pretty simplistic
;;; and are just enough to get through ansi-test.

(defclass unsigned-byte-8-transcoder (transcoder) ())

(defmethod read-element ((transcoder unsigned-byte-8-transcoder) stream)
  (let ((byte (stream-read-octet stream)))
    (if (eq byte :eof)
        :eof
        (coerce byte (stream-element-type stream)))))

(defmethod write-element ((transcoder unsigned-byte-8-transcoder) stream element)
  (stream-write-octet stream element)
  element)

(defmethod encoded-octet-length ((transcoder unsigned-byte-8-transcoder) elements)
  (length elements))

(defclass unsigned-byte-transcoder (transcoder)
  ((element-length :reader element-length
                :initarg :element-length
                :initform 1)
   (little-endian :reader little-endian
                  :initarg :little-endian
                  :initform nil)))

(defmethod read-element ((transcoder unsigned-byte-transcoder) stream)
  (loop with element = 0
        for pos from 0 below (element-length transcoder)
        for octet = (stream-read-octet stream)
        finally (return element)
        when (eq octet :eof)
          return (if (zerop pos)
                     :eof
                     (unexpected-eof transcoder stream))
        do (setf (ldb (byte 8 (* pos 8)) element) octet)))

(defmethod write-element ((transcoder unsigned-byte-transcoder) stream element)
  (loop for pos from 0 below (element-length transcoder)
        do (stream-write-octet stream (ldb (byte 8 (* pos 8)) element)))
  element)

(defmethod encoded-octet-length ((transcoder unsigned-byte-transcoder) elements)
  (* (element-length transcoder) (length elements)))

(defmethod make-transcoder
    ((external-format (eql :be)) element-type &rest options)
  (cond ((or (subtypep element-type '(unsigned-byte 8))
             (eq element-type 'unsigned-byte)
             (equal element-type '(unsigned-byte *))
             (subtypep element-type '(signed-byte 8))
             (eq element-type 'signed-byte)
             (equal element-type '(signed-byte *)))
         (apply #'make-instance 'unsigned-byte-8-transcoder
                options))
        ((subtypep element-type 'unsigned-byte)
         (loop for element-length from 2 upto 100
               finally (call-next-method)
               when (subtypep element-type `(unsigned-byte ,(* 8 element-length)))
                 return (apply #'make-instance 'unsigned-byte-transcoder
                               :element-length element-length
                               options)))
        ((subtypep element-type 'signed-byte)
         (loop for element-length from 2 upto 100
               finally (call-next-method)
               when (subtypep element-type `(signed-byte ,(* 8 element-length)))
                 return (apply #'make-instance 'unsigned-byte-transcoder
                               :element-length element-length
                               options)))
        (t
         (call-next-method))))
