(cl:in-package #:cyclosis)

(defclass ascii-transcoder (character-transcoder) ())

(defmethod read-element ((transcoder ascii-transcoder) stream)
  (let ((octet (stream-read-octet stream)))
    (if (eq octet :eof)
        :eof
        (coerce (code-char octet) (stream-element-type stream)))))

(defmethod write-element ((transcoder ascii-transcoder) stream element)
  (let ((code (char-code element)))
    (if (< code #x80)
        (stream-write-octet stream code)
        (invalid-element transcoder stream element)))
  element)

(defmethod encoded-length ((transcoder ascii-transcoder) elements)
  (length elements))

(defmethod make-transcoder
    ((external-format (eql :ascii)) element-type &rest options)
  (apply #'make-instance 'ascii-transcoder options))
