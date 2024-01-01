(cl:in-package #:cyclosis)

;;; Broadcast stream.

(defclass broadcast-stream (fundamental-output-stream)
  ((%streams :initarg :streams :reader broadcast-stream-streams)))

(defun make-broadcast-stream (&rest streams)
  (dolist (stream streams)
    (when (not (output-stream-p stream))
      (error 'type-error
             :expected-type '(satisfies output-stream-p)
             :datum stream)))
  (make-instance 'broadcast-stream :streams streams))

(defmacro broadcast-stream-op ((substream broadcast-stream default) &body body)
  `(loop with result = ,default
         for ,substream in (broadcast-stream-streams ,broadcast-stream)
         finally (return result)
         do (setf result (progn ,@body))))

(defmacro broadcast-last-stream-op ((substream broadcast-stream default) &body body)
  `(if (null (broadcast-stream-streams ,broadcast-stream))
       ,default
       (let ((,substream (car (last (broadcast-stream-streams ,broadcast-stream)))))
         ,@body)))

(defmethod stream-write-char ((stream broadcast-stream) character)
  (broadcast-stream-op (substream stream character)
    (stream-write-char substream character)))

(defmethod stream-write-byte ((stream broadcast-stream) byte)
  (broadcast-stream-op (substream stream byte)
    (stream-write-byte substream byte)))

(defmethod stream-write-sequence
    ((stream broadcast-stream) seq &optional (start 0) end)
  (broadcast-stream-op (substream stream start)
    (stream-write-sequence substream seq start end)))

(defmethod stream-element-type ((stream broadcast-stream))
  (broadcast-last-stream-op (substream stream t)
    (stream-element-type substream)))

(defmethod stream-fresh-line ((stream broadcast-stream))
  (broadcast-stream-op (substream stream nil)
    (stream-fresh-line substream)))

(defmethod stream-file-length ((stream broadcast-stream))
  (broadcast-last-stream-op (substream stream 0)
    (stream-file-length substream)))

(defmethod stream-file-string-length ((stream broadcast-stream) string)
  (broadcast-last-stream-op (substream stream 1)
    (stream-file-string-length substream string)))

(defmethod stream-file-position
    ((stream broadcast-stream) &optional (position-spec nil position-spec-p))
  (if position-spec-p
      (broadcast-last-stream-op (substream stream nil)
        (stream-file-position substream position-spec))
      (broadcast-last-stream-op (substream stream 0)
        (stream-file-position substream))))

(defmethod stream-line-column ((stream broadcast-stream))
  (broadcast-last-stream-op (substream stream nil)
    (stream-line-column substream)))

(defmethod stream-line-length ((stream broadcast-stream))
  (broadcast-last-stream-op (substream stream nil)
    (stream-line-length substream)))

(defmethod stream-advance-to-column ((stream broadcast-stream) column)
  (broadcast-stream-op (substream stream nil)
    (stream-advance-to-column substream column)))

(defmethod stream-clear-output ((stream broadcast-stream))
  (broadcast-stream-op (substream stream nil)
    (stream-clear-output substream)))

(defmethod stream-finish-output ((stream broadcast-stream))
  (broadcast-stream-op (substream stream nil)
    (stream-finish-output substream)))

(defmethod stream-force-output ((stream broadcast-stream))
  (broadcast-stream-op (substream stream nil)
    (stream-force-output substream)))

(defmethod stream-start-line-p ((stream broadcast-stream))
  (broadcast-last-stream-op (substream stream nil)
    (stream-start-line-p substream)))

(defmethod stream-terpri ((stream broadcast-stream))
  (broadcast-stream-op (substream stream nil)
    (stream-terpri substream)))

(defmethod stream-write-string
    ((stream broadcast-stream) string &optional (start 0) end)
  (broadcast-stream-op (substream stream string)
    (stream-write-string substream string start end)))

(defmethod stream-external-format ((stream broadcast-stream))
  (broadcast-last-stream-op (substream stream :default)
    (stream-external-format substream)))
