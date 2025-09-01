(cl:in-package #:cyclosis)

;;; Echo stream.

(defclass echo-stream (abstract-two-way-stream)
  ()
  (:documentation "An echo stream is a bidirectional stream that gets its input from an
associated input stream and sends its output to an associated output stream."))

(defmethod stream-read-byte ((stream echo-stream))
  (let ((c (call-next-method)))
    (when (integerp c)
      (stream-write-byte (stream-output-stream stream) c))
    c))

(defmethod stream-read-char ((stream echo-stream))
  (let ((c (call-next-method)))
    (when (characterp c)
      (stream-write-char (stream-output-stream stream) c))
    c))

(defmethod stream-read-char-no-hang ((stream echo-stream))
  (let ((c (call-next-method)))
    (when (characterp c)
      (stream-write-char (stream-output-stream stream) c))
    c))

(defmethod stream-read-sequence ((stream echo-stream) seq &optional (start 0) end)
  (let ((result (call-next-method)))
    (stream-write-sequence (stream-output-stream stream) seq start result)
    result))

(defmethod stream-read-line ((stream echo-stream))
  (multiple-value-bind (line missing-newline-p)
      (call-next-method)
    (stream-write-string (stream-output-stream stream) line)
    (unless missing-newline-p
      (stream-terpri (stream-output-stream stream)))
    (values line missing-newline-p)))
