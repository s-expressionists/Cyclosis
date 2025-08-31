(cl:in-package #:cyclosis)

;;; Synonym stream.

(defclass synonym-stream (fundamental-stream stream-symbol-mixin)
  ())

(defmethod print-object ((object synonym-stream) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "for ~S" (stream-symbol object))))

(defun follow-synonym-stream (stream)
  (symbol-value (stream-symbol stream)))

(defmethod stream-element-type ((stream synonym-stream))
  (stream-element-type (follow-synonym-stream stream)))

(defmethod (setf stream-element-type) (new-value (stream synonym-stream))
  (setf (stream-element-type (follow-synonym-stream stream)) new-value))

(defmethod stream-external-format ((stream synonym-stream))
  (stream-external-format (follow-synonym-stream stream)))

(defmethod (setf stream-external-format) (new-value (stream synonym-stream))
  (setf (stream-external-format (follow-synonym-stream stream)) new-value))

(defmethod close ((stream synonym-stream) &key abort)
  (close (follow-synonym-stream stream) :abort abort))

(defmethod open-stream-p ((stream synonym-stream))
  (open-stream-p (follow-synonym-stream stream)))

(defmethod input-stream-p ((stream synonym-stream))
  (input-stream-p (follow-synonym-stream stream)))

(defmethod output-stream-p ((stream synonym-stream))
  (output-stream-p (follow-synonym-stream stream)))

(defmethod interactive-stream-p ((stream synonym-stream))
  (interactive-stream-p (follow-synonym-stream stream)))

(defmethod stream-file-position ((stream synonym-stream))
  (stream-file-position (follow-synonym-stream stream)))

(defmethod (setf stream-file-position) (position-spec (stream synonym-stream))
  (setf (stream-file-position (follow-synonym-stream stream)) position-spec))

(defmethod stream-file-length ((stream synonym-stream))
  (stream-file-length (follow-synonym-stream stream)))

(defmethod stream-file-string-length ((stream synonym-stream) string)
  (stream-file-string-length (follow-synonym-stream stream) string))

(defmethod stream-clear-input ((stream synonym-stream))
  (stream-clear-input (follow-synonym-stream stream)))

(defmethod stream-read-sequence
    ((stream synonym-stream) seq &optional (start 0) end)
  (stream-read-sequence (follow-synonym-stream stream) seq start end))

(defmethod stream-clear-output ((stream synonym-stream))
  (stream-clear-output (follow-synonym-stream stream)))

(defmethod stream-finish-output ((stream synonym-stream))
  (stream-finish-output (follow-synonym-stream stream)))

(defmethod stream-force-output ((stream synonym-stream))
  (stream-force-output (follow-synonym-stream stream)))

(defmethod stream-write-sequence
    ((stream synonym-stream) seq &optional (start 0) end)
  (stream-write-sequence (follow-synonym-stream stream) seq start end))

(defmethod stream-read-byte ((stream synonym-stream))
  (stream-read-byte (follow-synonym-stream stream)))

(defmethod stream-write-byte ((stream synonym-stream) integer)
  (stream-write-byte (follow-synonym-stream stream) integer))

(defmethod stream-peek-char ((stream synonym-stream))
  (stream-peek-char (follow-synonym-stream stream)))

(defmethod stream-read-char-no-hang ((stream synonym-stream))
  (stream-read-char-no-hang (follow-synonym-stream stream)))

(defmethod stream-read-char ((stream synonym-stream))
  (stream-read-char (follow-synonym-stream stream)))

(defmethod stream-read-line ((stream synonym-stream))
  (stream-read-line (follow-synonym-stream stream)))

(defmethod stream-listen ((stream synonym-stream))
  (stream-listen (follow-synonym-stream stream)))

(defmethod stream-unread-char ((stream synonym-stream) character)
  (stream-unread-char (follow-synonym-stream stream) character))

(defmethod stream-advance-to-column ((stream synonym-stream) column)
  (stream-advance-to-column (follow-synonym-stream stream) column))

(defmethod stream-fresh-line ((stream synonym-stream))
  (stream-fresh-line (follow-synonym-stream stream)))

(defmethod stream-line-column ((stream synonym-stream))
  (stream-line-column (follow-synonym-stream stream)))

(defmethod stream-line-length ((stream synonym-stream))
  (stream-line-length (follow-synonym-stream stream)))

(defmethod stream-start-line-p ((stream synonym-stream))
  (stream-start-line-p (follow-synonym-stream stream)))

(defmethod stream-terpri ((stream synonym-stream))
  (stream-terpri (follow-synonym-stream stream)))

(defmethod stream-write-char ((stream synonym-stream) character)
  (stream-write-char (follow-synonym-stream stream) character))

(defmethod stream-write-string
    ((stream synonym-stream) string &optional (start 0) end)
  (stream-write-string (follow-synonym-stream stream) string start end))
