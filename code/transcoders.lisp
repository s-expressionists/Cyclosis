(cl:in-package #:cyclosis)

(define-condition transcode-error (stream-error)
  ())

(define-condition illegal-sequence (transcode-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Illegal sequence while decoding from ~s"
                     (stream-error-stream condition)))))

(define-condition unexpected-eof (transcode-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Unexpected EOF while decoding from ~s"
                     (stream-error-stream condition)))))

;;; utf-8 transcoder

(defclass utf-8-transcoder () ())

(defmethod read-element ((transcoder utf-8-transcoder) stream)
  (let (octet1 octet2 octet3 octet4)
    (cond ((eq (setf octet1 (stream-read-octet stream)) :eof)
           :eof)
          ((< octet1 #b10000000)
           (coerce (code-char octet1) (stream-element-type stream)))
          ((eq (setf octet2 (stream-read-octet input-stream)) :eof)
           (error 'unexpected-eof :stream stream))
          ((not (<= #b10000000 octet2 #b10111111))
           (error 'illegal-sequence :stream stream))
          ((< octet1 #b11100000)
           (coerce (code-char (+ (ash (- octet1 #b11000000) -2)
                                 (- octet2 #b10000000)))
                   (stream-element-type stream)))
          ((eq (setf octet3 (stream-read-octet input-stream)) :eof)
           (error 'unexpected-eof :stream stream))
          ((not (<= #b10000000 octet3 #b10111111))
           (error 'illegal-sequence :stream stream))
          ((< octet1 #b11110000)
           (coerce (code-char (+ (ash (- octet1 #b11100000) -4)
                                 (ash (- octet2 #b10000000) -2)
                                 (- octet3 #b10000000)))
                   (stream-element-type stream)))
          ((eq (setf octet4 (stream-read-octet input-stream)) :eof)
           (error 'unexpected-eof :stream stream))
          ((not (and (<= #b10000000 octet4 #b10111111)
                     (< octet1 #b11111000)))
           (error 'illegal-sequence :stream stream))
          (t
           (coerce (code-char (+ (ash (- octet1 #b11110000) -6)
                                 (ash (- octet2 #b10000000) -4)
                                 (ash (- octet3 #b10000000) -2)
                                 (- octet4 #b10000000)))
                   (stream-element-type stream))))))

(defmethod write-element ((transcoder utf-8-transcoder) stream element)
  (let ((code (char-code element))
        (m1 #b10000000)
        (m2 #b00111111))
    (cond ((< code #x80)
           (stream-write-octet stream code))
          ((< code #x800)
           (stream-write-octet
            stream
            (logior #b11000000 (ash code -6)))
           (stream-write-octet stream (logior m1 (logand code m2))))
          ((< code #x1000)
           (stream-write-octet stream (logior #b11100000 (ash code -12)))
           (stream-write-octet stream (logior m1 (logand (ash code -6) m2)))
           (stream-write-octet stream (logior m1 (logand code m2))))
          (t
           (stream-write-octet stream (logior #b11110000 (ash code -18)))
           (stream-write-octet stream (logior m1 (logand (ash code -12) m2)))
           (stream-write-octet stream (logior m1 (logand (ash code -6) m2)))
           (stream-write-octet stream (logior m1 (logand code m2))))))
  element)

(defun make-utf-8-transcoder (element-type external-format)
  (when (and (subtypep element-type 'character)
             (or (eq external-format :utf-8)
                 (eq external-format :default)))
    (values (make-instance 'utf-8-transcoder) element-type '(:utf-8 :lf))))

(pushnew #'make-utf-8-transcoder *octet-transcoders*)

;;; unsigned-byte-8-transcoder

(defclass unsigned-byte-8-transcoder () ())

(defmethod read-element ((transcoder unsigned-byte-8-transcoder) stream)
  (let ((byte (stream-read-octet stream)))
    (if (eq byte :eof)
        :eof
        (coerce byte (stream-element-type stream)))))

(defmethod write-element ((transcoder unsigned-byte-8-transcoder) stream element)
  (stream-write-octet stream element)
  element)

(defun make-unsigned-byte-8-transcoder (element-type external-format)
  (when (subtypep element-type '(unsigned-byte 8))
    (values (make-instance 'unsigned-byte-8-transcoder) element-type :default)))

(pushnew #'make-unsigned-byte-8-transcoder *octet-transcoders*)
