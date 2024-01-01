(cl:in-package #:cyclosis-extrinsic)

(defclass extrinsic-client ()
  ())

(defparameter *client* (make-instance 'extrinsic-client))

(defclass extrinsic-file-stream
    (cyclosis:file-stream
     cyclosis:fundamental-stream)
  ((target :reader target
           :initarg :target)))

(defmethod cyclosis:close ((stream extrinsic-file-stream) &key abort)
  (cl:close (target stream))
  (call-next-method))

(defmethod cyclosis:stream-read-char ((stream extrinsic-file-stream))
  (cl:read-char (target stream) nil :eof))

(defmethod cyclosis:stream-unread-char ((stream extrinsic-file-stream) character)
  (cl:unread-char character (target stream)))

(defmethod cyclosis:stream-read-char-no-hang ((stream extrinsic-file-stream))
  (cl:read-char-no-hang (target stream) nil))

(defmethod cyclosis:stream-peek-char ((stream extrinsic-file-stream))
  (cl:peek-char nil (target stream) nil :eof))

(defmethod cyclosis:stream-listen ((stream extrinsic-file-stream))
  (cl:listen (target stream)))

(defmethod cyclosis:stream-read-line ((stream extrinsic-file-stream))
  (cl:read-line (target stream) nil))

(defmethod cyclosis:stream-clear-input ((stream extrinsic-file-stream))
  (cl:clear-input (target stream)))

(defmethod cyclosis:stream-write-char ((stream extrinsic-file-stream) character)
  (cl:write-char character (target stream)))

(defmethod cyclosis:stream-write-string ((stream extrinsic-file-stream) string &optional start end)
  (cl:write-string string (target stream) :start (or start 0) :end end))

(defmethod cyclosis:stream-terpri ((stream extrinsic-file-stream))
  (cl:terpri (target stream)))

(defmethod cyclosis:stream-fresh-line ((stream extrinsic-file-stream))
  (cl:fresh-line (target stream)))

(defmethod cyclosis:stream-finish-output ((stream extrinsic-file-stream))
  (cl:finish-output (target stream)))

(defmethod cyclosis:stream-force-output ((stream extrinsic-file-stream))
  (cl:force-output (target stream)))

(defmethod cyclosis:open-stream-p ((stream extrinsic-file-stream))
  (cl:open-stream-p (target stream)))

(defmethod cyclosis:streamp ((stream extrinsic-file-stream))
  (cl:streamp (target stream)))

(defmethod cyclosis:input-stream-p ((stream extrinsic-file-stream))
  (cl:input-stream-p (target stream)))

(defmethod cyclosis:output-stream-p ((stream extrinsic-file-stream))
  (cl:output-stream-p (target stream)))

(defmethod cyclosis:stream-element-type ((stream extrinsic-file-stream))
  (cl:stream-element-type (target stream)))

(defmethod cyclosis:stream-read-byte ((stream extrinsic-file-stream))
  (cl:read-byte (target stream) nil nil))

(defmethod cyclosis:stream-write-byte ((stream extrinsic-file-stream) integer)
  (cl:write-byte integer (target stream)))

(defmethod cyclosis:make-file-stream
    ((client extrinsic-client) path direction
     if-exists if-does-not-exist element-type external-format)
  (make-instance 'extrinsic-file-stream
                 :target (cl:open path :direction direction
                                       :if-exists if-exists
                                       :if-does-not-exist if-does-not-exist
                                       :element-type element-type
                                       :external-format external-format)))

(defun replicate-stream (stream direction)
  (etypecase stream
    (cl:synonym-stream
     (replicate-stream (symbol-value (cl:synonym-stream-symbol stream))
                       direction))
    (cl:two-way-stream
     (ecase direction
       (:input
        (replicate-stream (cl:two-way-stream-input-stream stream)
                          :input))
       (:output
        (replicate-stream (cl:two-way-stream-output-stream stream)
                          :output))
       (:io
        (make-two-way-stream (replicate-stream (cl:two-way-stream-input-stream stream)
                                               :input)
                             (replicate-stream (cl:two-way-stream-output-stream stream)
                                               :output)))))
    (cl:echo-stream
     (ecase direction
       (:input
        (replicate-stream (cl:echo-stream-input-stream stream)
                          :input))
       (:output
        (replicate-stream (cl:echo-stream-output-stream stream)
                          :output))
       (:io
        (make-two-way-stre (replicate-stream (cl:echo-stream-input-stream stream)
                                             :input)
                           (replicate-stream (cl:echo-stream-output-stream stream)
                                             :output)))))
    (cl:concatenated-stream
     (ecase direction
       (:input
        (apply #'make-concatenated-stream
               (mapcar (lambda (stream)
                         (replicate-stream stream :input))
                       (cl:concatenated-stream-streams stream))))))
    (cl:broadcast-stream
     (ecase direction
       (:output
        (apply #'make-broadcast-stream
               (mapcar (lambda (stream)
                         (replicate-stream stream :output))
                       (cl:broadcast-stream-streams stream))))))
    (cl:stream
     (unless (or (and (eq direction :input)
                      (cl:input-stream-p stream))
                 (and (eq direction :output)
                      (cl:output-stream-p stream))
                 (and (eq direction :io)
                      (cl:input-stream-p stream)
                      (cl:output-stream-p stream)))
       (error "Stream ~s does not support direction ~s." stream direction))
     (make-instance 'extrinsic-file-stream :target stream))))

(defparameter *standard-input* (replicate-stream cl:*standard-input* :input))

(defparameter *standard-output* (replicate-stream cl:*standard-output* :output))

(defparameter *terminal-io* (replicate-stream cl:*terminal-io* :io))

(defmethod cyclosis:coerce-input-stream
    ((client extrinsic-client) (designator null))
  *standard-input*)

(defmethod cyclosis:coerce-input-stream
    ((client extrinsic-client) (designator (eql t)))
  *terminal-io*)

(defmethod cyclosis:coerce-output-stream
    ((client extrinsic-client) (designator null))
  *standard-output*)

(defmethod cyclosis:coerce-output-stream
    ((client extrinsic-client) (designator (eql t)))
  *terminal-io*)

(cyclosis:define-interface *client*)

