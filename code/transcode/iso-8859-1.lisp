(cl:in-package #:cyclosis)

(defclass iso-8859-1-transcoder (character-transcoder) ())

(defmethod read-element ((transcoder iso-8859-1-transcoder) stream)
  (let ((octet (stream-read-octet stream)))
    (if (eq octet :eof)
        :eof
        (coerce (code-char octet) (stream-element-type stream)))))

(defmethod write-element ((transcoder iso-8859-1-transcoder) stream element)
  (let ((code (char-code element)))
    (if (< code #x100)
        (stream-write-octet stream code)
        (invalid-element transcoder stream element)))
  element)

(defmethod encoded-length ((transcoder iso-8859-1-transcoder) elements)
  (length elements))

(defmethod make-transcoder
    ((external-format (eql :iso-8859-1)) element-type &rest options)
  (apply #'make-instance 'iso-8859-1-transcoder options))
