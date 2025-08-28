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

(defmethod trinsic:initial-cell-value
    ((client extrinsic-client) (name (eql 'cl:*standard-input*)) (type (eql 'cl:variable)))
  (cyclosis:make-synonym-stream '+standard-input+))

(defmethod trinsic:initial-cell-value
    ((client extrinsic-client) (name (eql 'cl:*standard-output*)) (type (eql 'cl:variable)))
  (cyclosis:make-synonym-stream '+standard-output+))

(defmethod trinsic:initial-cell-value
    ((client extrinsic-client) (name (eql 'cl:*error-output*)) (type (eql 'cl:variable)))
  (cyclosis:make-synonym-stream '+error-output+))

(defmethod trinsic:initial-cell-value
    ((client extrinsic-client) (name (eql 'cl:*trace-output*)) (type (eql 'cl:variable)))
  (cyclosis:make-synonym-stream '+error-output+))

(defmethod trinsic:initial-cell-value
    ((client extrinsic-client) (name (eql 'cl:*terminal-io*)) (type (eql 'cl:variable)))
  (cyclosis:make-synonym-stream '+terminal-io+))

(defmethod trinsic:initial-cell-value
    ((client extrinsic-client) (name (eql 'cl:*query-io*)) (type (eql 'cl:variable)))
  (cyclosis:make-synonym-stream '+terminal-io+))

(defmethod trinsic:initial-cell-value
    ((client extrinsic-client) (name (eql 'cl:*debug-io*)) (type (eql 'cl:variable)))
  (cyclosis:make-synonym-stream '+terminal-io+))

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

(cyclosis:define-interface :client-class intrinsic-client :client-form *client* :intrinsic t)
