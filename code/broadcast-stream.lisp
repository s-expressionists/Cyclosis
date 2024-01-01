(cl:in-package #:cyclosis)

;;; Broadcast stream.

(defclass broadcast-stream (fundamental-output-stream)
  ((%streams :initarg :streams :reader broadcast-stream-streams)))

(defun make-broadcast-stream (&rest streams)
  (dolist (stream streams)
    (when (not (and (streamp stream)
                    (output-stream-p stream)))
      (error 'type-error
             :expected-type 'output-stream
             :datum stream)))
  (make-instance 'broadcast-stream :streams streams))

(defmacro broadcast-stream-op ((substream broadcast-stream default) &body body)
  `(loop
      with result = ,default
      for ,substream in (broadcast-stream-streams ,broadcast-stream)
      do (setf result (progn ,@body))
      finally (return result)))

(defmethod stream-write-char ((stream broadcast-stream) character)
  (broadcast-stream-op (substream stream character)
    (write-char character substream)))

(defmethod stream-write-byte ((stream broadcast-stream) byte)
  (broadcast-stream-op (substream stream byte)
    (write-byte byte substream)))

(defmethod stream-write-sequence
    ((stream broadcast-stream) seq &optional (start 0) end)
  (broadcast-stream-op (substream stream start)
    (write-sequence seq substream :start start :end end)))

(defmethod stream-element-type ((stream broadcast-stream))
  (broadcast-stream-op (substream stream t)
    (stream-element-type substream)))

(defmethod stream-fresh-line ((stream broadcast-stream))
  (broadcast-stream-op (substream stream nil)
    (fresh-line substream)))

(defmethod stream-file-length ((stream broadcast-stream))
  (broadcast-stream-op (substream stream 0)
    (file-length substream)))

(defmethod stream-file-string-length ((stream broadcast-stream) string)
  (broadcast-stream-op (substream stream 1)
    (file-string-length substream)))

(defmethod stream-file-position
    ((stream broadcast-stream) &optional (position-spec nil position-spec-p))
  (if position-spec-p
      (broadcast-stream-op (substream stream nil)
        (file-position substream position-spec))
      (broadcast-stream-op (substream stream 0)
        (file-position substream))))

(defmethod stream-line-column ((stream broadcast-stream))
  (broadcast-stream-op (substream stream nil)
    (line-column substream)))

(defmethod stream-line-length ((stream broadcast-stream))
  (broadcast-stream-op (substream stream nil)
    (line-length substream)))

(defmethod stream-advance-to-column ((stream broadcast-stream) column)
  (broadcast-stream-op (substream stream nil)
    (advance-to-column column substream)))

(defmethod stream-clear-output ((stream broadcast-stream))
  (broadcast-stream-op (substream stream nil)
    (clear-output substream)))

(defmethod stream-finish-output ((stream broadcast-stream))
  (broadcast-stream-op (substream stream nil)
    (finish-output substream)))

(defmethod stream-force-output ((stream broadcast-stream))
  (broadcast-stream-op (substream stream nil)
    (force-output substream)))

(defmethod stream-start-line-p ((stream broadcast-stream))
  (broadcast-stream-op (substream stream nil)
    (start-line-p substream)))

(defmethod stream-terpri ((stream broadcast-stream))
  (broadcast-stream-op (substream stream nil)
    (terpri substream)))

(defmethod stream-write-string
    ((stream broadcast-stream) string &optional (start 0) end)
  (broadcast-stream-op (substream stream string)
    (write-string string substream :start start :end end)))

(defmethod stream-external-format ((stream broadcast-stream))
  (broadcast-stream-op (substream stream :default)
    (stream-external-format substream)))
