(cl:in-package #:cyclosis)

(defclass abstract-two-way-stream
    (fundamental-input-stream fundamental-output-stream character-input-mixin)
  ((%input-stream :accessor stream-input-stream
                  :initarg :input-stream
                  :type input-stream)
   (%output-stream :accessor stream-output-stream
                   :initarg :output-stream
                   :type output-stream)))

(defmethod initialize-instance :after
    ((instance abstract-two-way-stream) &rest initargs &key)
  (declare (ignore initargs))
  (check-input-stream (stream-input-stream instance))
  (check-output-stream (stream-output-stream instance)))

;;; Character input

(defmethod stream-read-char ((stream abstract-two-way-stream))
  (stream-read-char (stream-input-stream stream)))

(defmethod stream-read-char-no-hang ((stream abstract-two-way-stream))
  (stream-read-char-no-hang (stream-input-stream stream)))

(defmethod stream-peek-char ((stream abstract-two-way-stream))
  (stream-peek-char (stream-input-stream stream)))

(defmethod stream-listen ((stream abstract-two-way-stream))
  (stream-listen (stream-input-stream stream)))

(defmethod stream-read-line ((stream abstract-two-way-stream))
  (stream-read-line (stream-input-stream stream)))

(defmethod stream-clear-input ((stream abstract-two-way-stream))
  (stream-clear-input (stream-input-stream stream)))

;;; Character output

(defmethod stream-write-char ((stream abstract-two-way-stream) character)
  (stream-write-char (stream-output-stream stream) character))

(defmethod stream-line-column ((stream abstract-two-way-stream))
  (stream-line-column (stream-output-stream stream)))

(defmethod (setf stream-line-column) (new-value (stream abstract-two-way-stream))
  (setf (stream-line-column (stream-output-stream stream)) new-value))

(defmethod stream-line-number ((stream abstract-two-way-stream))
  (stream-line-column (stream-output-stream stream)))

(defmethod (setf stream-line-number) (new-value (stream abstract-two-way-stream))
  (setf (stream-line-number (stream-output-stream stream)) new-value))

(defmethod stream-start-line-p ((stream abstract-two-way-stream))
  (stream-start-line-p (stream-output-stream stream)))

(defmethod stream-write-string ((stream abstract-two-way-stream) string &optional (start 0) end)
  (stream-write-string (stream-output-stream stream) string start end))

(defmethod stream-terpri ((stream abstract-two-way-stream))
  (stream-terpri (stream-output-stream stream)))

(defmethod stream-fresh-line ((stream abstract-two-way-stream))
  (stream-fresh-line (stream-output-stream stream)))

(defmethod stream-finish-output ((stream abstract-two-way-stream))
  (stream-finish-output (stream-output-stream stream)))

(defmethod stream-force-output ((stream abstract-two-way-stream))
  (stream-force-output (stream-output-stream stream)))

(defmethod stream-clear-output ((stream abstract-two-way-stream))
  (stream-clear-output (stream-output-stream stream)))

(defmethod stream-advance-to-column ((stream abstract-two-way-stream) column)
  (stream-advance-to-column (stream-output-stream stream) column))

;;; Other functions

(defmethod stream-element-type ((stream abstract-two-way-stream))
  (let ((in (stream-element-type (stream-input-stream stream)))
        (out (stream-element-type (stream-output-stream stream))))
    (if (or (equal in out)
            (and (subtypep in out)
                 (subtypep out in)))
        in
        `(and ,in ,out))))

(defmethod (setf stream-element-type) (new-value (stream abstract-two-way-stream))
  (setf (stream-element-type (stream-input-stream stream)) new-value
        (stream-element-type (stream-output-stream stream)) new-value))

;;; Binary streams

(defmethod stream-read-byte ((stream abstract-two-way-stream))
  (stream-read-byte (stream-input-stream stream)))

(defmethod stream-write-byte ((stream abstract-two-way-stream) byte)
  (stream-write-byte (stream-output-stream stream) byte))

;;; Extensions to Gray Streams

(defmethod interactive-stream-p ((stream abstract-two-way-stream))
  (interactive-stream-p (stream-input-stream stream)))

(defmethod stream-read-sequence ((stream abstract-two-way-stream) seq &optional (start 0) end)
  (stream-read-sequence (stream-input-stream stream) seq start end))

(defmethod stream-write-sequence ((stream abstract-two-way-stream) seq &optional (start 0) end)
  (stream-write-sequence (stream-output-stream stream) seq start end))

(defmethod stream-line-length ((stream abstract-two-way-stream))
  (stream-line-length (stream-output-stream stream)))

(defmethod (setf stream-line-length) (new-value (stream abstract-two-way-stream))
  (setf (stream-line-length (stream-output-stream stream)) new-value))

(defmethod stream-input-column ((stream abstract-two-way-stream))
  (stream-input-column (stream-input-stream stream)))

(defmethod stream-input-line ((stream abstract-two-way-stream))
  (stream-input-line (stream-input-stream stream)))

;;; Two-way stream.

(defclass two-way-stream (abstract-two-way-stream)
  ())
