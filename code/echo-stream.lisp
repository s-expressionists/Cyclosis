(cl:in-package #:cyclosis)

;;; Echo stream.

(defclass echo-stream (fundamental-output-stream
                       fundamental-input-stream
                       character-input-mixin)
  ((input-stream :initarg :input-stream
                 :reader echo-stream-input-stream)
   (output-stream :initarg :output-stream
                  :reader echo-stream-output-stream)))

(defun make-echo-stream (input-stream output-stream)
  (check-input-stream input-stream)
  (check-output-stream output-stream)
  (make-instance 'echo-stream
                 :input-stream input-stream
                 :output-stream output-stream))

(defmethod stream-element-type ((stream echo-stream))
  (let ((in (stream-element-type (echo-stream-input-stream stream)))
        (out (stream-element-type (echo-stream-output-stream stream))))
    (if (or (equal in out)
            (and (subtypep in out)
                 (subtypep out in)))
        in
        `(and ,in ,out))))

(defmethod stream-write-byte ((stream echo-stream) character)
  (stream-write-byte (echo-stream-output-stream stream) character))

(defmethod stream-write-char ((stream echo-stream) character)
  (stream-write-char (echo-stream-output-stream stream) character))

(defmethod stream-write-sequence ((stream echo-stream) seq &optional (start 0) end)
  (stream-write-sequence (echo-stream-output-stream stream) seq start end))

(defmethod stream-read-byte ((stream echo-stream))
  (let ((c (stream-read-byte (echo-stream-input-stream stream))))
    (when (integerp c)
      (stream-write-byte (echo-stream-output-stream stream) c))
    c))

(defmethod stream-read-char ((stream echo-stream))
  (let ((c (stream-read-char (echo-stream-input-stream stream))))
    (when (characterp c)
      (stream-write-char (echo-stream-output-stream stream) c))
    c))

(defmethod stream-read-char-no-hang ((stream echo-stream))
  (let ((c (stream-read-char-no-hang (echo-stream-input-stream stream))))
    (when (characterp c)
      (stream-write-char (echo-stream-output-stream stream) c))
    c))

(defmethod stream-read-sequence ((stream echo-stream) seq &optional (start 0) end)
  (let ((result (stream-read-sequence (echo-stream-input-stream stream) seq start end)))
    (stream-write-sequence (echo-stream-output-stream stream) seq start result)
    result))

(defmethod stream-listen ((stream echo-stream))
  (stream-listen (echo-stream-input-stream stream)))

(defmethod stream-clear-input ((stream echo-stream))
  (stream-clear-input (echo-stream-input-stream stream)))

(defmethod stream-clear-output ((stream echo-stream))
  (stream-clear-output (echo-stream-output-stream stream)))

(defmethod stream-finish-output ((stream echo-stream))
  (stream-finish-output (echo-stream-output-stream stream)))

(defmethod stream-force-output ((stream echo-stream))
  (stream-force-output (echo-stream-output-stream stream)))

(defmethod stream-peek-char ((stream echo-stream))
  ;; Don't echo when peeking.
  (stream-peek-char (echo-stream-input-stream stream)))

(defmethod stream-read-line ((stream echo-stream))
  (multiple-value-bind (line missing-newline-p)
      (stream-read-line (echo-stream-input-stream stream))
    (if missing-newline-p
        (stream-write-string (echo-stream-output-stream stream) line)
        (stream-write-line (echo-stream-output-stream stream) line))
    (values line missing-newline-p)))

(defmethod stream-fresh-line ((stream echo-stream))
  (stream-fresh-line (echo-stream-output-stream stream)))

(defmethod stream-line-column ((stream echo-stream))
  (stream-line-column (echo-stream-output-stream stream)))

(defmethod stream-advance-to-column ((stream echo-stream) column)
  (stream-advance-to-column (echo-stream-output-stream stream) column))

(defmethod stream-line-length ((stream echo-stream))
  (stream-line-length (echo-stream-output-stream stream)))

(defmethod stream-start-line-p ((stream echo-stream))
  (stream-start-line-p (echo-stream-output-stream stream)))

(defmethod stream-terpri ((stream echo-stream))
  (stream-terpri (echo-stream-output-stream stream)))

(defmethod stream-write-string
    ((stream echo-stream) string &optional (start 0) end)
  (stream-write-string (echo-stream-output-stream stream) string start end))
