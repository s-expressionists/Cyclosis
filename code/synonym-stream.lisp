(cl:in-package #:cyclosis)

;;; Synonym stream.

(defclass synonym-stream (fundamental-stream)
  ((%symbol :initarg :symbol :reader synonym-stream-symbol)))

(defun make-synonym-stream (symbol)
  (check-type symbol symbol)
  (make-instance 'synonym-stream :symbol symbol))

(defmethod print-object ((object synonym-stream) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "for ~S" (synonym-stream-symbol object))))

(defun follow-synonym-stream (stream)
  (symbol-value (synonym-stream-symbol stream)))

(defmethod stream-element-type ((stream synonym-stream))
  (stream-element-type (follow-synonym-stream stream)))

(defmethod stream-external-format ((stream synonym-stream))
  (stream-external-format (follow-synonym-stream stream)))

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

(defmethod stream-file-position
    ((stream synonym-stream) &optional (position-spec nil position-spec-p))
  (if position-spec-p
      (file-position (follow-synonym-stream stream) position-spec)
      (file-position (follow-synonym-stream stream))))

(defmethod stream-file-length ((stream synonym-stream))
  (file-length (follow-synonym-stream stream)))

(defmethod stream-file-string-length ((stream synonym-stream) string)
  (file-string-length (follow-synonym-stream stream) string))

(defmethod stream-clear-input ((stream synonym-stream))
  (clear-input (follow-synonym-stream stream)))

(defmethod stream-read-sequence
    ((stream synonym-stream) seq &optional (start 0) end)
  (read-sequence seq (follow-synonym-stream stream) :start start :end end))

(defmethod stream-clear-output ((stream synonym-stream))
  (clear-output (follow-synonym-stream stream)))

(defmethod stream-finish-output ((stream synonym-stream))
  (finish-output (follow-synonym-stream stream)))

(defmethod stream-force-output ((stream synonym-stream))
  (force-output (follow-synonym-stream stream)))

(defmethod stream-write-sequence
    ((stream synonym-stream) seq &optional (start 0) end)
  (write-sequence seq (follow-synonym-stream stream) :start start :end end))

(defmethod stream-read-byte ((stream synonym-stream))
  (read-byte (follow-synonym-stream stream) nil :eof))

(defmethod stream-write-byte ((stream synonym-stream) integer)
  (write-byte integer (follow-synonym-stream stream)))

(defmethod stream-peek-char ((stream synonym-stream))
  (peek-char nil (follow-synonym-stream stream) nil :eof))

(defmethod stream-read-char-no-hang ((stream synonym-stream))
  (read-char-no-hang (follow-synonym-stream stream) nil :eof))

(defmethod stream-read-char ((stream synonym-stream))
  (read-char (follow-synonym-stream stream) nil :eof))

(defmethod stream-read-line ((stream synonym-stream))
  (read-line (follow-synonym-stream stream) nil ""))

(defmethod stream-listen ((stream synonym-stream))
  (listen (follow-synonym-stream stream)))

(defmethod stream-unread-char ((stream synonym-stream) character)
  (unread-char character (follow-synonym-stream stream)))

(defmethod stream-advance-to-column ((stream synonym-stream) column)
  (advance-to-column column (follow-synonym-stream stream)))

(defmethod stream-fresh-line ((stream synonym-stream))
  (fresh-line (follow-synonym-stream stream)))

(defmethod stream-line-column ((stream synonym-stream))
  (line-column (follow-synonym-stream stream)))

(defmethod stream-line-length ((stream synonym-stream))
  (line-length (follow-synonym-stream stream)))

(defmethod stream-start-line-p ((stream synonym-stream))
  (start-line-p (follow-synonym-stream stream)))

(defmethod stream-terpri ((stream synonym-stream))
  (terpri (follow-synonym-stream stream)))

(defmethod stream-write-char ((stream synonym-stream) character)
  (write-char character (follow-synonym-stream stream)))

(defmethod stream-write-string
    ((stream synonym-stream) string &optional (start 0) end)
  (write-string string (follow-synonym-stream stream) :start start :end end))
