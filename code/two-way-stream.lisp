(cl:in-package #:cyclosis)

;;; Two-way stream.

(defclass two-way-stream (fundamental-output-stream
                          fundamental-input-stream
                          unread-char-mixin)
  ((input-stream :initarg :input-stream
                 :reader two-way-stream-input-stream)
   (output-stream :initarg :output-stream
                  :reader two-way-stream-output-stream)))

(defun make-two-way-stream (input-stream output-stream)
  (when (not (and (streamp input-stream)
                  (input-stream-p input-stream)))
    (error 'type-error :datum input-stream :expected-type 'input-stream))
  (when (not (and (streamp output-stream)
                  (output-stream-p output-stream)))
    (error 'type-error :datum output-stream :expected-type 'output-stream))
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
  (write-byte byte (two-way-stream-output-stream stream)))

(defmethod stream-write-char ((stream two-way-stream) character)
  (write-char character (two-way-stream-output-stream stream)))

(defmethod stream-write-sequence ((stream two-way-stream) seq &optional (start 0) end)
  (write-sequence seq (two-way-stream-output-stream stream) :start start :end end))

(defmethod stream-read-byte ((stream two-way-stream))
  (read-byte (two-way-stream-input-stream stream) nil :eof))

(defmethod stream-read-char ((stream two-way-stream))
  (read-char (two-way-stream-input-stream stream) nil :eof))

(defmethod stream-read-char-no-hang ((stream two-way-stream))
  (read-char-no-hang (two-way-stream-input-stream stream) nil :eof))

(defmethod stream-read-sequence ((stream two-way-stream) seq &optional (start 0) end)
  (read-sequence seq (two-way-stream-input-stream stream) :start start :end end))

(defmethod stream-listen ((stream two-way-stream))
  (listen (two-way-stream-input-stream stream)))

(defmethod stream-clear-input ((stream two-way-stream))
  (clear-input (two-way-stream-input-stream stream)))

(defmethod stream-clear-output ((stream two-way-stream))
  (clear-output (two-way-stream-output-stream stream)))

(defmethod stream-finish-output ((stream two-way-stream))
  (finish-output (two-way-stream-output-stream stream)))

(defmethod stream-force-output ((stream two-way-stream))
  (force-output (two-way-stream-output-stream stream)))

(defmethod stream-peek-char ((stream two-way-stream))
  (peek-char nil (two-way-stream-input-stream stream) nil :eof))

(defmethod stream-read-line ((stream two-way-stream))
  (read-line (two-way-stream-input-stream stream) nil ""))

(defmethod stream-fresh-line ((stream two-way-stream))
  (fresh-line (two-way-stream-output-stream stream)))

(defmethod stream-line-column ((stream two-way-stream))
  (line-column (two-way-stream-output-stream stream)))

(defmethod stream-advance-to-column ((stream two-way-stream) column)
  (advance-to-column column (two-way-stream-output-stream stream)))

(defmethod stream-line-length ((stream two-way-stream))
  (line-length (two-way-stream-output-stream stream)))

(defmethod stream-start-line-p ((stream two-way-stream))
  (start-line-p (two-way-stream-output-stream stream)))

(defmethod stream-terpri ((stream two-way-stream))
  (terpri (two-way-stream-output-stream stream)))

(defmethod stream-write-string
    ((stream two-way-stream) string &optional (start 0) end)
  (write-string string (two-way-stream-output-stream stream) :start start :end end))
