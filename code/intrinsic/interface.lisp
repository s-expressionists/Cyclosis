(cl:in-package #:cyclosis-intrinsic)

(defclass intrinsic-client ()
  ())

(defparameter *client*
  (make-instance 'intrinsic-client))

(defparameter +standard-input+
  (make-instance 'cyclosis:posix-file-stream
                 :descriptor 0
                 :input t
                 :close-descriptor nil
                 :element-type 'character))

(defparameter +standard-output+
  (make-instance 'cyclosis:posix-file-stream
                 :descriptor 1
                 :output t
                 :close-descriptor nil
                 :element-type 'character))

(defparameter +error-output+
  (make-instance 'cyclosis:posix-file-stream
                 :descriptor 2
                 :output t
                 :close-descriptor nil
                 :element-type 'character))

(defparameter +terminal-io+
  (cyclosis:make-two-way-stream +standard-input+ +standard-output+))

(defparameter *standard-input* (cyclosis:make-synonym-stream '+standard-input+))

(defparameter *standard-output* (cyclosis:make-synonym-stream '+standard-output+))

(defparameter *error-output* (cyclosis:make-synonym-stream '+error-output+))

(defparameter *trace-output* (cyclosis:make-synonym-stream '+error-output+))

(defparameter *terminal-io* (cyclosis:make-synonym-stream '+terminal-io+))

(defparameter *debug-io* (cyclosis:make-synonym-stream '+terminal-io+))

(defparameter *query-io* *debug-io*)

(defmethod cyclosis:state-value ((client intrinsic-client) (aspect (eql 'cl:*standard-input*)))
  *standard-input*)

(defmethod cyclosis:state-value ((client intrinsic-client) (aspect (eql 'cl:*standard-output*)))
  *standard-output*)

(defmethod cyclosis:state-value ((client intrinsic-client) (aspect (eql 'cl:*error-output*)))
  *error-output*)

(defmethod cyclosis:state-value ((client intrinsic-client) (aspect (eql 'cl:*trace-output*)))
  *trace-output*)

(defmethod cyclosis:state-value ((client intrinsic-client) (aspect (eql 'cl:*terminal-io*)))
  *terminal-io*)

(defmethod cyclosis:state-value ((client intrinsic-client) (aspect (eql 'cl:*debug-io*)))
  *debug-io*)

(defmethod cyclosis:state-value ((client intrinsic-client) (aspect (eql 'cl:*query-io*)))
  *query-io*)

(defmethod cyclosis:state-value ((client intrinsic-client) (aspect (eql 'cl:format)))
  #'format)

(defmethod cyclosis:whitespace-char-p ((client intrinsic-client) ch)
  #+ccl (ccl::whitespacep ch)
  #+clasp
    (eq (core:syntax-type *readtable* ch) :whitespace)
  #+cmucl (lisp::whitespacep ch)
  #+(and ecl (not bytecode))
    (ffi::c-inline (ch) (t) :bool
                   "ecl_readtable_get(ecl_current_readtable(), ECL_CHAR_CODE(#0), NULL) == cat_whitespace"
                            :one-liner t)
  #+sicl (eq (eclector.readtable:syntax-type *readtable* ch) :whitespace)
  #+sbcl (sb-impl::whitespace[2]p ch *readtable*)
  #-(or ccl clasp cmucl (and ecl (not bytecode)) sbcl)
    (and (member ch '(#\tab #\newline #\linefeed #\page #\return #\space))
         t))

(cyclosis:define-interface *client* :intrinsic t)
