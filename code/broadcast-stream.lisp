(cl:in-package #:cyclosis)

;;; Broadcast stream.

(defclass broadcast-stream (fundamental-output-stream)
  ((%output-streams :accessor stream-output-streams
                    :initarg :output-streams
                    :type list))
  (:documentation "A broadcast stream is an output stream which has associated with it a set of
zero or more output streams such that any output sent to the broadcast stream gets passed on as
output to each of the associated output streams. (If a broadcast stream has no component
streams, then all output to the broadcast stream is discarded.)"))

(defmethod initialize-instance :after ((instance broadcast-stream) &rest initargs &key)
  (declare (ignore initargs))
  (mapc #'check-output-stream (stream-output-streams instance)))

(defmacro broadcast-stream-op ((substream broadcast-stream default) &body body)
  `(loop with result = ,default
         for ,substream in (stream-output-streams ,broadcast-stream)
         finally (return result)
         do (setf result (progn ,@body))))

(defmacro broadcast-last-stream-op ((substream broadcast-stream default) &body body)
  `(if (null (stream-output-streams ,broadcast-stream))
       ,default
       (let ((,substream (car (last (stream-output-streams ,broadcast-stream)))))
         ,@body)))

;;; Character output

(defmethod stream-write-char ((stream broadcast-stream) character)
  (broadcast-stream-op (substream stream character)
    (stream-write-char substream character)))

(defmethod stream-line-column ((stream broadcast-stream))
  (broadcast-last-stream-op (substream stream nil)
    (stream-line-column substream)))

(defmethod stream-start-line-p ((stream broadcast-stream))
  (broadcast-last-stream-op (substream stream nil)
    (stream-start-line-p substream)))

(defmethod stream-write-string ((stream broadcast-stream) string &optional (start 0) end)
  (broadcast-stream-op (substream stream string)
    (stream-write-string substream string start end)))

(defmethod stream-terpri ((stream broadcast-stream))
  (broadcast-stream-op (substream stream nil)
    (stream-terpri substream)))

(defmethod stream-fresh-line ((stream broadcast-stream))
  (broadcast-stream-op (substream stream nil)
    (stream-fresh-line substream)))

(defmethod stream-finish-output ((stream broadcast-stream))
  (broadcast-stream-op (substream stream nil)
    (stream-finish-output substream)))

(defmethod stream-force-output ((stream broadcast-stream))
  (broadcast-stream-op (substream stream nil)
    (stream-force-output substream)))

(defmethod stream-clear-output ((stream broadcast-stream))
  (broadcast-stream-op (substream stream nil)
    (stream-clear-output substream)))

(defmethod stream-advance-to-column ((stream broadcast-stream) column)
  (broadcast-stream-op (substream stream nil)
    (stream-advance-to-column substream column)))

;;; Other functions

(defmethod stream-element-type ((stream broadcast-stream))
  (broadcast-last-stream-op (substream stream t)
    (stream-element-type substream)))

;;; Binary streams

(defmethod stream-write-byte ((stream broadcast-stream) byte)
  (broadcast-stream-op (substream stream byte)
    (stream-write-byte substream byte)))

;;; Extensions to Gray Streams

(defmethod stream-external-format ((stream broadcast-stream))
  (broadcast-last-stream-op (substream stream :default)
    (stream-external-format substream)))

(defmethod stream-file-position ((stream broadcast-stream))
  (broadcast-last-stream-op (substream stream 0)
    (stream-file-position substream)))

(defmethod stream-file-length ((stream broadcast-stream))
  (broadcast-last-stream-op (substream stream 0)
    (stream-file-length substream)))

(defmethod stream-file-string-length ((stream broadcast-stream) string)
  (broadcast-last-stream-op (substream stream 1)
    (stream-file-string-length substream string)))

(defmethod stream-write-sequence ((stream broadcast-stream) seq &optional (start 0) end)
  (broadcast-stream-op (substream stream start)
    (stream-write-sequence substream seq start end)))

(defmethod stream-line-length ((stream broadcast-stream))
  (broadcast-last-stream-op (substream stream nil)
    (stream-line-length substream)))

