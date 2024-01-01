(cl:in-package #:cyclosis)

;;; Echo stream.

(defclass echo-stream (fundamental-output-stream
                       fundamental-input-stream
                       unread-char-mixin)
  ((input-stream :initarg :input-stream
                 :reader echo-stream-input-stream)
   (output-stream :initarg :output-stream
                  :reader echo-stream-output-stream)))

(defun make-echo-stream (input-stream output-stream)
  (when (not (and (streamp input-stream)
                  (input-stream-p input-stream)))
    (error 'type-error :datum input-stream :expected-type 'input-stream))
  (when (not (and (streamp output-stream)
                  (output-stream-p output-stream)))
    (error 'type-error :datum output-stream :expected-type 'output-stream))
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
  (write-byte character (echo-stream-output-stream stream)))

(defmethod stream-write-char ((stream echo-stream) character)
  (write-char character (echo-stream-output-stream stream)))

(defmethod stream-write-sequence ((stream echo-stream) seq &optional (start 0) end)
  (write-sequence seq (echo-stream-output-stream stream) :start start :end end))

(defmethod stream-read-byte ((stream echo-stream))
  (let ((c (read-byte (echo-stream-input-stream stream) nil :eof)))
    (when (integerp c)
      (write-byte c (echo-stream-output-stream stream)))
    c))

(defmethod stream-read-char ((stream echo-stream))
  (let ((c (read-char (echo-stream-input-stream stream) nil :eof)))
    (when (characterp c)
      (write-char c (echo-stream-output-stream stream)))
    c))

(defmethod stream-read-char-no-hang ((stream echo-stream))
  (let ((c (read-char-no-hang (echo-stream-input-stream stream) nil :eof)))
    (when (characterp c)
      (write-char c (echo-stream-output-stream stream)))
    c))

(defmethod stream-read-sequence ((stream echo-stream) seq &optional (start 0) end)
  (let ((result (read-sequence seq (echo-stream-input-stream stream) :start start :end end)))
    (write-sequence seq (echo-stream-output-stream stream) :start start :end result)
    result))

(defmethod stream-listen ((stream echo-stream))
  (listen (echo-stream-input-stream stream)))

(defmethod stream-clear-input ((stream echo-stream))
  (clear-input (echo-stream-input-stream stream)))

(defmethod stream-clear-output ((stream echo-stream))
  (clear-output (echo-stream-output-stream stream)))

(defmethod stream-finish-output ((stream echo-stream))
  (finish-output (echo-stream-output-stream stream)))

(defmethod stream-force-output ((stream echo-stream))
  (force-output (echo-stream-output-stream stream)))

(defmethod stream-peek-char ((stream echo-stream))
  ;; Don't echo when peeking.
  (peek-char nil (echo-stream-input-stream stream) nil :eof))

(defmethod stream-read-line ((stream echo-stream))
  (multiple-value-bind (line missing-newline-p)
      (read-line (echo-stream-input-stream stream) nil "")
    (if missing-newline-p
        (write-string line (echo-stream-output-stream stream))
        (write-line line (echo-stream-output-stream stream)))
    (values line missing-newline-p)))

(defmethod stream-fresh-line ((stream echo-stream))
  (fresh-line (echo-stream-output-stream stream)))

(defmethod stream-line-column ((stream echo-stream))
  (line-column (echo-stream-output-stream stream)))

(defmethod stream-advance-to-column ((stream echo-stream) column)
  (advance-to-column column (echo-stream-output-stream stream)))

(defmethod stream-line-length ((stream echo-stream))
  (line-length (echo-stream-output-stream stream)))

(defmethod stream-start-line-p ((stream echo-stream))
  (start-line-p (echo-stream-output-stream stream)))

(defmethod stream-terpri ((stream echo-stream))
  (terpri (echo-stream-output-stream stream)))

(defmethod stream-write-string
    ((stream echo-stream) string &optional (start 0) end)
  (write-string string (echo-stream-output-stream stream) :start start :end end))
