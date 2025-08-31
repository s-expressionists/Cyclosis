(cl:in-package #:cyclosis-extrinsic)

#+sbcl (declaim (sb-ext:muffle-conditions sb-c::&optional-and-&key-in-lambda-list))

(defclass extrinsic-client ()
  ())

(defparameter *client* (make-instance 'extrinsic-client))

(defclass extrinsic-file-stream
    (cyclosis:file-stream
     cyclosis:fundamental-stream)
  ((target :reader target
           :initarg :target)))

(defmethod cyclosis:close ((stream extrinsic-file-stream) &key abort)
  (cl:close (target stream) :abort abort)
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

(defmethod cyclosis:stream-file-position ((stream extrinsic-file-stream))
  (cl:file-position (target stream)))

(defmethod (setf cyclosis:stream-file-position) (new-value (stream extrinsic-file-stream))
  (if (cl:file-position (target stream) new-value)
      new-value
      (error 'stream-error :stream stream)))

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

#-sbcl
(defmethod cyclosis:make-file-stream
    ((client extrinsic-client) path direction
     if-exists if-does-not-exist element-type external-format)
  (when (eq if-does-not-exist :create)
    (ensure-directories-exist path))
  (let ((target (cl:open path :direction direction
                              :if-exists if-exists
                              :if-does-not-exist if-does-not-exist
                              :element-type element-type
                              :external-format external-format)))
    (if target
        (make-instance 'extrinsic-file-stream
                       :target target)
        nil)))

(defgeneric replicate-stream (stream direction)
  (:method ((stream cl:synonym-stream) direction)
    (replicate-stream (symbol-value (cl:synonym-stream-symbol stream))
                      direction))
  (:method ((stream cl:two-way-stream) (direction (eql :input)))
    (replicate-stream (cl:two-way-stream-input-stream stream) :input))
  (:method ((stream cl:two-way-stream) (direction (eql :output)))
    (replicate-stream (cl:two-way-stream-output-stream stream) :output))
  (:method ((stream cl:two-way-stream) (direction (eql :io)))
    (make-instance 'cyclosis:two-way-stream
                   :input-stream (replicate-stream (cl:two-way-stream-input-stream stream)
                                                   :input)
                   :output-stream (replicate-stream (cl:two-way-stream-output-stream stream)
                                                    :output)))
  (:method ((stream cl:echo-stream) (direction (eql :input)))
    (replicate-stream (cl:echo-stream-input-stream stream) :input))
  (:method ((stream cl:echo-stream) (direction (eql :output)))
    (replicate-stream (cl:echo-stream-output-stream stream) :output))
  (:method ((stream cl:echo-stream) (direction (eql :io)))
    (make-instance 'cyclosis:echo-stream
                   :input-stream (replicate-stream (cl:echo-stream-input-stream stream) :input)
                   :output-stream (replicate-stream (cl:echo-stream-output-stream stream)
                                                    :output)))
  (:method ((stream cl:concatenated-stream) (direction (eql :input)))
    (make-instance 'cyclosis:concatenated-stream
                   :input-streams (mapcar (lambda (stream)
                                            (replicate-stream stream :input))
                                          (cl:concatenated-stream-streams stream))))
  (:method ((stream cl:broadcast-stream) (direction (eql :output)))
    (make-instance 'cyclosis:broadcast-stream
                   :output-streams (mapcar (lambda (stream)
                                             (replicate-stream stream :output))
                                           (cl:broadcast-stream-streams stream))))
  (:method ((stream cl:stream) direction)
    (unless (or (and (eq direction :input)
                     (cl:input-stream-p stream))
                (and (eq direction :output)
                     (cl:output-stream-p stream))
                (and (eq direction :io)
                     (cl:input-stream-p stream)
                     (cl:output-stream-p stream)))
      (error "Stream ~s does not support direction ~s." stream direction))
    (make-instance 'extrinsic-file-stream :target stream)))

(defmethod trinsic:initial-cell-value
    ((client extrinsic-client) (name (eql 'cl:*standard-input*)) (type (eql 'cl:variable)))
  (replicate-stream cl:*standard-input* :input))

(defmethod trinsic:initial-cell-value
    ((client extrinsic-client) (name (eql 'cl:*standard-output*)) (type (eql 'cl:variable)))
  (replicate-stream cl:*standard-output* :output))

(defmethod trinsic:initial-cell-value
    ((client extrinsic-client) (name (eql 'cl:*error-output*)) (type (eql 'cl:variable)))
  (replicate-stream cl:*error-output* :output))

(defmethod trinsic:initial-cell-value
    ((client extrinsic-client) (name (eql 'cl:*trace-output*)) (type (eql 'cl:variable)))
  (replicate-stream cl:*trace-output* :output))

(defmethod trinsic:initial-cell-value
    ((client extrinsic-client) (name (eql 'cl:*terminal-io*)) (type (eql 'cl:variable)))
  (replicate-stream cl:*terminal-io* :io))

(defmethod trinsic:initial-cell-value
    ((client extrinsic-client) (name (eql 'cl:*query-io*)) (type (eql 'cl:variable)))
  (replicate-stream cl:*query-io* :io))

(defmethod trinsic:initial-cell-value
    ((client extrinsic-client) (name (eql 'cl:*debug-io*)) (type (eql 'cl:variable)))
  (replicate-stream cl:*debug-io* :io))

(defmethod cyclosis:whitespace-char-p ((client extrinsic-client) ch)
  #+abcl (java:jcall "isWhitespace" *readtable* ch)
  #+ccl (ccl::whitespacep ch)
  #+clasp
    (eq (core:syntax-type *readtable* ch) :whitespace)
  #+cmucl (lisp::whitespacep ch)
  #+(and ecl (not bytecode))
    (ffi::c-inline (ch) (t) :bool
                   "ecl_readtable_get(ecl_current_readtable(), ECL_CHAR_CODE(#0), NULL) == cat_whitespace"
                            :one-liner t)
  #+sbcl (sb-impl::whitespace[2]p ch *readtable*)
  #-(or abcl ccl clasp cmucl (and ecl (not bytecode)) sbcl)
    (and (member ch '(#\tab #\newline #\linefeed #\page #\return #\space))
         t))

(cyclosis:define-interface :client-form *client* :client-class extrinsic-client)

(defun extrinsic-format (destination control-string &rest args)
  (when (eq destination t)
    (setf destination *standard-output*))
  (cond ((or (null destination)
             (stringp destination))
         (apply 'format destination control-string args))
        (t
         ;; TODO: Need to preserve column and line length
         (cyclosis:stream-write-string destination
                                       (apply 'format nil control-string args))
         nil)))

(defmethod trinsic:cell-value
    ((client extrinsic-client) (name (eql 'cl:format)) (type (eql 'cl:function)))
  #'extrinsic-format)
