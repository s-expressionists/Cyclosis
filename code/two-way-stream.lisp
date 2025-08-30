(cl:in-package #:cyclosis)

;;; Two-way stream.

(defclass two-way-stream
    (fundamental-output-stream fundamental-input-stream character-input-mixin
     stream-input-stream-mixin stream-output-stream-mixin)
  ())

(defmethod stream-read-char ((stream two-way-stream))
  (check-open-stream stream)
  (stream-read-char (stream-input-stream stream)))

(defmethod stream-unread-char ((stream two-way-stream) character)
  (check-open-stream stream)
  (stream-unread-char (stream-input-stream stream) character))

(defmethod stream-read-char-no-hang ((stream two-way-stream))
  (check-open-stream stream)
  (stream-read-char-no-hang (stream-input-stream stream)))

(defmethod stream-peek-char ((stream two-way-stream))
  (check-open-stream stream)
  (stream-peek-char (stream-input-stream stream)))

(defmethod stream-listen ((stream two-way-stream))
  (check-open-stream stream)
  (stream-listen (stream-input-stream stream)))

(defmethod stream-read-line ((stream two-way-stream))
  (check-open-stream stream)
  (stream-read-line (stream-input-stream stream)))

(defmethod stream-clear-input ((stream two-way-stream))
  (check-open-stream stream)
  (stream-clear-input (stream-input-stream stream)))

(defmethod stream-write-char ((stream two-way-stream) character)
  (check-open-stream stream)
  (stream-write-char (stream-output-stream stream) character))

(defmethod stream-line-column ((stream two-way-stream))
  (stream-line-column (stream-output-stream stream)))

(defmethod (setf stream-line-column) (new-value (stream two-way-stream))
  (setf (stream-line-column (stream-output-stream stream))
        new-value))

(defmethod stream-line-number ((stream two-way-stream))
  (stream-line-number (stream-output-stream stream)))

(defmethod (setf stream-line-number) (new-value (stream two-way-stream))
  (setf (stream-line-number (stream-output-stream stream))
        new-value))

(defmethod stream-start-line-p ((stream two-way-stream))
  (stream-start-line-p (stream-output-stream stream)))

(defmethod stream-write-string
    ((stream two-way-stream) string &optional (start 0) end)
  (stream-write-string (stream-output-stream stream) string start end))

(defmethod stream-terpri ((stream two-way-stream))
  (check-open-stream stream)
  (stream-terpri (stream-output-stream stream)))

(defmethod stream-fresh-line ((stream two-way-stream))
  (check-open-stream stream)
  (stream-fresh-line (stream-output-stream stream)))

(defmethod stream-finish-output ((stream two-way-stream))
  (check-open-stream stream)
  (stream-finish-output (stream-output-stream stream)))

(defmethod stream-force-output ((stream two-way-stream))
  (check-open-stream stream)
  (stream-force-output (stream-output-stream stream)))

(defmethod stream-clear-output ((stream two-way-stream))
  (check-open-stream stream)
  (stream-clear-output (stream-output-stream stream)))

(defmethod stream-advance-to-column ((stream two-way-stream) column)
  (check-open-stream stream)
  (stream-advance-to-column (stream-output-stream stream) column))

(defmethod stream-element-type ((stream two-way-stream))
  (let ((in (stream-element-type (stream-input-stream stream)))
        (out (stream-element-type (stream-output-stream stream))))
    (if (or (equal in out)
            (and (subtypep in out)
                 (subtypep out in)))
        in
        `(and ,in ,out))))

(defmethod (setf stream-element-type) (new-value stream)
  (setf (stream-element-type (stream-input-stream stream)) new-value
        (stream-element-type (stream-output-stream stream)) new-value))

(defmethod stream-read-byte ((stream two-way-stream))
  (check-open-stream stream)
  (stream-read-byte (stream-input-stream stream)))

(defmethod stream-write-byte ((stream two-way-stream) byte)
  (check-open-stream stream)
  (stream-write-byte (stream-output-stream stream) byte))

(defmethod interactive-stream-p ((stream two-way-stream))
  (interactive-stream-p (stream-input-stream stream)))

(defmethod stream-read-sequence ((stream two-way-stream) seq &optional (start 0) end)
  (check-open-stream stream)
  (stream-read-sequence (stream-input-stream stream) seq start end))

(defmethod stream-write-sequence ((stream two-way-stream) seq &optional (start 0) end)
  (check-open-stream stream)
  (stream-write-sequence (stream-output-stream stream) seq start end))

(defmethod stream-line-length ((stream two-way-stream))
  (stream-line-length (stream-output-stream stream)))
