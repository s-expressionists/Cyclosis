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
  (cl:read-char-no-hang (target stream) nil :eof))

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

(defmethod cyclosis:stream-clear-output ((stream extrinsic-file-stream))
  (cl:clear-output (target stream)))

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

(defmethod cyclosis:stream-external-format ((stream extrinsic-file-stream))
  (cl:stream-external-format (target stream)))

(defmethod cyclosis:stream-read-byte ((stream extrinsic-file-stream))
  (cl:read-byte (target stream) nil :eof))

(defmethod cyclosis:stream-write-byte ((stream extrinsic-file-stream) integer)
  (cl:write-byte integer (target stream)))

(defmethod cyclosis:stream-file-length ((stream extrinsic-file-stream))
  (cl:file-length (target stream)))

(defmethod cyclosis:stream-file-position
    ((stream extrinsic-file-stream) &optional position)
  (if position
      (cl:file-position (target stream) position)
      (cl:file-position (target stream))))

(defmethod cyclosis:stream-file-string-length ((stream extrinsic-file-stream) object)
  (cl:file-string-length (target stream) object))

(defmethod cyclosis:stream-write-sequence
    ((stream extrinsic-file-stream) sequence &optional (start 0) end)
  (cl:write-sequence sequence (target stream) :start start :end end))

(defmethod cyclosis:stream-read-sequence
    ((stream extrinsic-file-stream) sequence &optional (start 0) end)
  (cl:read-sequence sequence (target stream) :start start :end end))

(defmethod cyclosis:pathname ((stream extrinsic-file-stream))
  (cl:pathname (target stream)))

(defmethod cyclosis:truename ((stream extrinsic-file-stream))
  (cl:truename (target stream)))

(defmethod cyclosis:make-file-stream
    ((client extrinsic-client) path direction
     if-exists if-does-not-exist element-type external-format)
  (let ((target (cl:open path :direction direction
                              :if-exists if-exists
                              :if-does-not-exist if-does-not-exist
                              :element-type element-type
                              :external-format external-format)))
    (if target
        (make-instance 'extrinsic-file-stream
                       :target target)
        nil)))

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
        (cyclosis:make-two-way-stream (replicate-stream (cl:two-way-stream-input-stream stream)
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
        (cyclosis:make-two-way-stream (replicate-stream (cl:echo-stream-input-stream stream)
                                                        :input)
                                      (replicate-stream (cl:echo-stream-output-stream stream)
                                                        :output)))))
    (cl:concatenated-stream
     (ecase direction
       (:input
        (apply #'cyclosis:make-concatenated-stream
               (mapcar (lambda (stream)
                         (replicate-stream stream :input))
                       (cl:concatenated-stream-streams stream))))))
    (cl:broadcast-stream
     (ecase direction
       (:output
        (apply #'cyclosis:make-broadcast-stream
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

(defparameter *error-output* (replicate-stream cl:*error-output* :output))

(defparameter *trace-output* (replicate-stream cl:*trace-output* :output))

(defparameter *terminal-io* (replicate-stream cl:*terminal-io* :io))

(defparameter *debug-io* (replicate-stream cl:*debug-io* :io))

(defparameter *query-io* (replicate-stream cl:*query-io* :io))

(defmethod cyclosis:state-value ((client extrinsic-client) (aspect (eql 'cl:*standard-input*)))
  *standard-input*)

(defmethod cyclosis:state-value ((client extrinsic-client) (aspect (eql 'cl:*standard-output*)))
  *standard-output*)

(defmethod cyclosis:state-value ((client extrinsic-client) (aspect (eql 'cl:*error-output*)))
  *error-output*)

(defmethod cyclosis:state-value ((client extrinsic-client) (aspect (eql 'cl:*trace-output*)))
  *trace-output*)

(defmethod cyclosis:state-value ((client extrinsic-client) (aspect (eql 'cl:*terminal-io*)))
  *terminal-io*)

(defmethod cyclosis:state-value ((client extrinsic-client) (aspect (eql 'cl:*debug-io*)))
  *debug-io*)

(defmethod cyclosis:state-value ((client extrinsic-client) (aspect (eql 'cl:*query-io*)))
  *query-io*)

(defmethod cyclosis:whitespace-char-p ((client extrinsic-client) ch)
  #+ccl (ccl::whitespacep ch)
  #+clasp
    (eq (core:syntax-type char) :whitespace)
  #+cmucl (lisp::whitespacep ch)
  #+(and ecl (not bytecode))
    (ffi::c-inline (ch) (t) :bool
                   "ecl_readtable_get(ecl_current_readtable(), ECL_CHAR_CODE(#0), NULL) == cat_whitespace"
                            :one-liner t)
  #+sbcl (sb-impl::whitespace[2]p ch *readtable*)
  #-(or ccl clasp cmucl (and ecl (not bytecode)) sbcl)
    (and (member char '(#\tab #\newline #\linefeed #\page #\return #\space))
         t))

(cyclosis:define-interface *client*)
