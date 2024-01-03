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
  (check-input-stream input-stream)
  (check-output-stream output-stream)
  (make-instance 'two-way-stream
                 :input-stream input-stream
                 :output-stream output-stream))

(defmethod stream-read-char ((stream two-way-stream))
  (check-open-stream stream)
  (stream-read-char (two-way-stream-input-stream stream)))

(defmethod stream-unread-char ((stream two-way-stream) character)
  (check-open-stream stream)
  (stream-unread-char (two-way-stream-input-stream stream) character))

(defmethod stream-read-char-no-hang ((stream two-way-stream))
  (check-open-stream stream)
  (stream-read-char-no-hang (two-way-stream-input-stream stream)))

(defmethod stream-peek-char ((stream two-way-stream))
  (check-open-stream stream)
  (stream-peek-char (two-way-stream-input-stream stream)))

(defmethod stream-listen ((stream two-way-stream))
  (check-open-stream stream)
  (stream-listen (two-way-stream-input-stream stream)))

(defmethod stream-read-line ((stream two-way-stream))
  (check-open-stream stream)
  (stream-read-line (two-way-stream-input-stream stream)))

(defmethod stream-clear-input ((stream two-way-stream))
  (check-open-stream stream)
  (stream-clear-input (two-way-stream-input-stream stream)))

(defmethod stream-write-char ((stream two-way-stream) character)
  (check-open-stream stream)
  (stream-write-char (two-way-stream-output-stream stream) character))

(defmethod stream-line-column ((stream two-way-stream))
  (stream-line-column (two-way-stream-output-stream stream)))

(defmethod (setf stream-line-column) (new-value (stream two-way-stream))
  (setf (stream-line-column (two-way-stream-output-stream stream))
        new-value))

(defmethod stream-line-number ((stream two-way-stream))
  (stream-line-number (two-way-stream-output-stream stream)))

(defmethod (setf stream-line-number) (new-value (stream two-way-stream))
  (setf (stream-line-number (two-way-stream-output-stream stream))
        new-value))

(defmethod stream-start-line-p ((stream two-way-stream))
  (stream-start-line-p (two-way-stream-output-stream stream)))

(defmethod stream-write-string
    ((stream two-way-stream) string &optional (start 0) end)
  (stream-write-string (two-way-stream-output-stream stream) string start end))

(defmethod stream-terpri ((stream two-way-stream))
  (check-open-stream stream)
  (stream-terpri (two-way-stream-output-stream stream)))

(defmethod stream-fresh-line ((stream two-way-stream))
  (check-open-stream stream)
  (stream-fresh-line (two-way-stream-output-stream stream)))

(defmethod stream-finish-output ((stream two-way-stream))
  (check-open-stream stream)
  (stream-finish-output (two-way-stream-output-stream stream)))

(defmethod stream-force-output ((stream two-way-stream))
  (check-open-stream stream)
  (stream-force-output (two-way-stream-output-stream stream)))

(defmethod stream-clear-output ((stream two-way-stream))
  (check-open-stream stream)
  (stream-clear-output (two-way-stream-output-stream stream)))

(defmethod stream-advance-to-column ((stream two-way-stream) column)
  (check-open-stream stream)
  (stream-advance-to-column (two-way-stream-output-stream stream) column))

(defmethod stream-element-type ((stream two-way-stream))
  (let ((in (stream-element-type (two-way-stream-input-stream stream)))
        (out (stream-element-type (two-way-stream-output-stream stream))))
    (if (or (equal in out)
            (and (subtypep in out)
                 (subtypep out in)))
        in
        `(and ,in ,out))))

(defmethod (setf stream-element-type) (new-value stream)
  (setf (stream-element-type (two-way-stream-input-stream stream)) new-value
        (stream-element-type (two-way-stream-output-stream stream)) new-value))

(defmethod stream-read-byte ((stream two-way-stream))
  (check-open-stream stream)
  (stream-read-byte (two-way-stream-input-stream stream)))

(defmethod stream-write-byte ((stream two-way-stream) byte)
  (check-open-stream stream)
  (stream-write-byte (two-way-stream-output-stream stream) byte))

(defmethod interactive-stream-p ((stream two-way-stream))
  (interactive-stream-p (two-way-stream-input-stream stream)))

(defmethod stream-read-sequence ((stream two-way-stream) seq &optional (start 0) end)
  (check-open-stream stream)
  (stream-read-sequence (two-way-stream-input-stream stream) seq start end))

(defmethod stream-write-sequence ((stream two-way-stream) seq &optional (start 0) end)
  (check-open-stream stream)
  (stream-write-sequence (two-way-stream-output-stream stream) seq start end))

(defmethod stream-line-length ((stream two-way-stream))
  (stream-line-length (two-way-stream-output-stream stream)))
