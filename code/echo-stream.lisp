(cl:in-package #:cyclosis)

;;; Echo stream.

(defclass echo-stream
    (fundamental-output-stream fundamental-input-stream character-input-mixin
     stream-input-stream-mixin stream-output-stream-mixin)
  ()
  (:documentation "An echo stream is a bidirectional stream that gets its input from an
associated input stream and sends its output to an associated output stream."))

(defmethod stream-element-type ((stream echo-stream))
  (let ((in (stream-element-type (stream-input-stream stream)))
        (out (stream-element-type (stream-output-stream stream))))
    (if (or (equal in out)
            (and (subtypep in out)
                 (subtypep out in)))
        in
        `(and ,in ,out))))

(defmethod stream-write-byte ((stream echo-stream) character)
  (stream-write-byte (stream-output-stream stream) character))

(defmethod stream-write-char ((stream echo-stream) character)
  (stream-write-char (stream-output-stream stream) character))

(defmethod stream-write-sequence ((stream echo-stream) seq &optional (start 0) end)
  (stream-write-sequence (stream-output-stream stream) seq start end))

(defmethod stream-read-byte ((stream echo-stream))
  (let ((c (stream-read-byte (stream-input-stream stream))))
    (when (integerp c)
      (stream-write-byte (stream-output-stream stream) c))
    c))

(defmethod stream-read-char ((stream echo-stream))
  (let ((c (stream-read-char (stream-input-stream stream))))
    (when (characterp c)
      (stream-write-char (stream-output-stream stream) c))
    c))

(defmethod stream-read-char-no-hang ((stream echo-stream))
  (let ((c (stream-read-char-no-hang (stream-input-stream stream))))
    (when (characterp c)
      (stream-write-char (stream-output-stream stream) c))
    c))

(defmethod stream-read-sequence ((stream echo-stream) seq &optional (start 0) end)
  (let ((result (stream-read-sequence (stream-input-stream stream) seq start end)))
    (stream-write-sequence (stream-output-stream stream) seq start result)
    result))

(defmethod stream-listen ((stream echo-stream))
  (stream-listen (stream-input-stream stream)))

(defmethod stream-clear-input ((stream echo-stream))
  (stream-clear-input (stream-input-stream stream)))

(defmethod stream-clear-output ((stream echo-stream))
  (stream-clear-output (stream-output-stream stream)))

(defmethod stream-finish-output ((stream echo-stream))
  (stream-finish-output (stream-output-stream stream)))

(defmethod stream-force-output ((stream echo-stream))
  (stream-force-output (stream-output-stream stream)))

(defmethod stream-peek-char ((stream echo-stream))
  ;; Don't echo when peeking.
  (stream-peek-char (stream-input-stream stream)))

(defmethod stream-read-line ((stream echo-stream))
  (multiple-value-bind (line missing-newline-p)
      (stream-read-line (stream-input-stream stream))
    (stream-write-string (stream-output-stream stream) line)
    (unless missing-newline-p
      (stream-terpri (stream-output-stream stream)))
    (values line missing-newline-p)))

(defmethod stream-fresh-line ((stream echo-stream))
  (stream-fresh-line (stream-output-stream stream)))

(defmethod stream-line-column ((stream echo-stream))
  (stream-line-column (stream-output-stream stream)))

(defmethod stream-advance-to-column ((stream echo-stream) column)
  (stream-advance-to-column (stream-output-stream stream) column))

(defmethod stream-line-length ((stream echo-stream))
  (stream-line-length (stream-output-stream stream)))

(defmethod stream-start-line-p ((stream echo-stream))
  (stream-start-line-p (stream-output-stream stream)))

(defmethod stream-terpri ((stream echo-stream))
  (stream-terpri (stream-output-stream stream)))

(defmethod stream-write-string
    ((stream echo-stream) string &optional (start 0) end)
  (stream-write-string (stream-output-stream stream) string start end))
