(cl:in-package #:cyclosis)

;;; Two-way stream.

(defclass two-way-stream (fundamental-output-stream
                          fundamental-input-stream
                          character-input-mixin)
  ((input-stream :initarg :input-stream
                 :reader two-way-stream-input-stream)
   (output-stream :initarg :output-stream
                  :reader two-way-stream-output-stream)))

(defun make-two-way-stream (input-stream output-stream)
  (unless (input-stream-p input-stream)
    (error 'type-error :datum input-stream :expected-type '(satisfies input-stream-p)))
  (unless (output-stream-p output-stream)
    (error 'type-error :datum output-stream :expected-type '(satisfies output-stream-p)))
  (make-instance 'two-way-stream
                 :input-stream input-stream
                 :output-stream output-stream))

(defmethod stream-element-type ((stream two-way-stream))
  (let ((in (stream-element-type (two-way-stream-input-stream stream)))
        (out (stream-element-type (two-way-stream-output-stream stream))))
    (if (or (equal in out)
            (and (subtypep in out)
                 (subtypep out in)))
        in
        `(and ,in ,out))))

(defmethod stream-write-byte ((stream two-way-stream) byte)
  (stream-write-byte (two-way-stream-output-stream stream) byte))

(defmethod stream-write-char ((stream two-way-stream) character)
  (stream-write-char (two-way-stream-output-stream stream) character))

(defmethod stream-write-sequence ((stream two-way-stream) seq &optional (start 0) end)
  (stream-write-sequence (two-way-stream-output-stream stream) seq start end))

(defmethod stream-read-byte ((stream two-way-stream))
  (stream-read-byte (two-way-stream-input-stream stream)))

(defmethod stream-read-char ((stream two-way-stream))
  (stream-read-char (two-way-stream-input-stream stream)))

(defmethod stream-read-char-no-hang ((stream two-way-stream))
  (stream-read-char-no-hang (two-way-stream-input-stream stream)))

(defmethod stream-read-sequence ((stream two-way-stream) seq &optional (start 0) end)
  (stream-read-sequence (two-way-stream-input-stream stream) seq start end))

(defmethod stream-listen ((stream two-way-stream))
  (stream-listen (two-way-stream-input-stream stream)))

(defmethod stream-clear-input ((stream two-way-stream))
  (stream-clear-input (two-way-stream-input-stream stream)))

(defmethod stream-clear-output ((stream two-way-stream))
  (stream-clear-output (two-way-stream-output-stream stream)))

(defmethod stream-finish-output ((stream two-way-stream))
  (stream-finish-output (two-way-stream-output-stream stream)))

(defmethod stream-force-output ((stream two-way-stream))
  (stream-force-output (two-way-stream-output-stream stream)))

(defmethod stream-peek-char ((stream two-way-stream))
  (stream-peek-char (two-way-stream-input-stream stream)))

(defmethod stream-read-line ((stream two-way-stream))
  (stream-read-line (two-way-stream-input-stream stream)))

(defmethod stream-fresh-line ((stream two-way-stream))
  (stream-fresh-line (two-way-stream-output-stream stream)))

(defmethod stream-line-column ((stream two-way-stream))
  (stream-line-column (two-way-stream-output-stream stream)))

(defmethod stream-advance-to-column ((stream two-way-stream) column)
  (stream-advance-to-column (two-way-stream-output-stream stream) column))

(defmethod stream-line-length ((stream two-way-stream))
  (stream-line-length (two-way-stream-output-stream stream)))

(defmethod stream-start-line-p ((stream two-way-stream))
  (stream-start-line-p (two-way-stream-output-stream stream)))

(defmethod stream-terpri ((stream two-way-stream))
  (stream-terpri (two-way-stream-output-stream stream)))

(defmethod stream-write-string
    ((stream two-way-stream) string &optional (start 0) end)
  (stream-write-string (two-way-stream-output-stream stream) string start end))
