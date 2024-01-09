(cl:in-package #:cyclosis)

(defclass utf-32-transcoder (character-transcoder)
  ((buffer :reader buffer
           :initform (make-array 4 :element-type '(unsigned-byte 8)))
   (little-endian :reader little-endian
                  :initform nil
                  :initarg :little-endian)))

(defmethod read-element ((transcoder utf-32-transcoder) stream)
  (with-accessors ((buffer buffer)
                   (little-endian little-endian))
      transcoder
    (let ((count (stream-read-octets stream buffer)))
      (cond ((zerop count)
             :eof)
            ((< count 4)
             (apply #'unexpected-eof transcoder stream (coerce buffer 'list)))
            (little-endian
             (code-char (logior (ash (aref buffer 3) 24)
                                (ash (aref buffer 2) 16)
                                (ash (aref buffer 1) 8)
                                (aref buffer 0))))
            (t
             (code-char (logior (ash (aref buffer 0) 24)
                                (ash (aref buffer 1) 16)
                                (ash (aref buffer 2) 8)
                                (aref buffer 3))))))))

(defmethod write-element ((transcoder utf-32-transcoder) stream element)
  (let ((code (char-code element)))
    (cond ((little-endian transcoder)
           (stream-write-octet stream (ldb (byte 8 0) code))
           (stream-write-octet stream (ldb (byte 8 8) code))
           (stream-write-octet stream (ldb (byte 8 16) code))
           (stream-write-octet stream (ldb (byte 8 24) code)))
          (t
           (stream-write-octet stream (ldb (byte 8 24) code))
           (stream-write-octet stream (ldb (byte 8 16) code))
           (stream-write-octet stream (ldb (byte 8 8) code))
           (stream-write-octet stream (ldb (byte 8 0) code)))))
  element)

(defmethod encoded-length ((transcoder utf-32-transcoder) elements)
  (* 4 (length elements)))

(defmethod make-transcoder
    ((external-format (eql :utf-32)) element-type &rest options)
  (apply #'make-instance 'utf-32-transcoder options))

(defmethod make-transcoder
    ((external-format (eql :utf-32le)) element-type &rest options)
  (apply #'make-instance 'utf-32-transcoder :little-endian t options))
