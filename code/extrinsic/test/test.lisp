(in-package #:cyclosis-extrinsic/test)

;; The uses of CL:FORMAT in the streams tests are all cases in which
;; CL:WRITE-LINE or CL:WRITE-STRING would have been fine. Some people
;; rely on FORMAT too much.
(defun format (destination control-string &rest args)
  (if (or (null destination)
          (stringp destination))
      (apply #'cl:format destination control-string args)
      (cyclosis-extrinsic:write-string (apply #'cl:format nil control-string args)
                                       (if (eql destination t)
                                           cyclosis-extrinsic:*standard-output*
                                           destination))))

;; MAKE-CONCATENATED-STREAM.1 makes a call to READ on an empty stream.
;; Loading an extrinsic READ just for this one call would be difficult
;; and in this case READ-CHAR is just as effective.
(defun read (&optional input-stream (eof-error-p t) eof-value recursive-p)
  (if (cyclosis:streamp input-stream)
      (cyclosis-extrinsic:read-char input-stream eof-error-p eof-value)
      (cl:read input-stream eof-error-p eof-value recursive-p)))

(defvar *tests*
  '("BROADCAST-STREAM" "CLEAR-INPUT" "CLEAR-OUTPUT" "CONCATENATED-STREAM" "ECHO-STREAM"
    "FILE-LENGTH" "FILE-POSITION" "FILE-STRING-LENGTH" "FINISH-OUTPUT" "FORCE-OUTPUT"
    "FRESH-LINE" "GET-OUTPUT-STREAM-STRING" "INPUT-STREAM-P" "INTERACTIVE-STREAM-P"
    "LISTEN" "MAKE-BROADCAST" "MAKE-CONCATENATED" "MAKE-ECHO" "MAKE-STRING-INPUT"
    "MAKE-STRING-OUTPUT" "MAKE-SYNONYM" "MAKE-TWO-WAY" "OPEN" "OUTPUT-STREAM-P"
    "PEEK-CHAR" "READ-BYTE" "READ-CHAR" "READ-CHAR-NO-HANG" "READ-LINE" "READ-SEQUENCE"
    "STREAM-ELEMENT-TYPE" "STREAM-ERROR-STREAM" "STREAM-EXTERNAL-FORMAT" "STREAMP"
    "SYNONYM-STREAM" "TERPRI" "TWO-WAY-STREAM" "UNREAD-CHAR" "WITH-INPUT-FROM-STRING"
    "WITH-OPEN" "WITH-OUTPUT-TO-STRING" "WRITE-BYTE" "WRITE-CHAR" "WRITE-LINE"
    "WRITE-SEQUENCE" "WRITE-STRING"))

(defvar *symbols*
  '(format read
    cyclosis-extrinsic:*debug-io* cyclosis-extrinsic:*error-output*
    cyclosis-extrinsic:*query-io* cyclosis-extrinsic:*standard-input*
    cyclosis-extrinsic:*standard-output* cyclosis-extrinsic:*terminal-io*
    cyclosis-extrinsic:*trace-output* cyclosis-extrinsic:advance-to-column
    cyclosis-extrinsic:broadcast-stream cyclosis-extrinsic:broadcast-stream-streams
    cyclosis-extrinsic:clear-input cyclosis-extrinsic:clear-output
    cyclosis-extrinsic:close cyclosis-extrinsic:concatenated-stream
    cyclosis-extrinsic:concatenated-stream-streams cyclosis-extrinsic:echo-stream
    cyclosis-extrinsic:echo-stream-input-stream cyclosis-extrinsic:echo-stream-output-stream
    cyclosis-extrinsic:file-length cyclosis-extrinsic:file-position
    cyclosis-extrinsic:file-stream cyclosis-extrinsic:file-string-length
    cyclosis-extrinsic:finish-output cyclosis-extrinsic:force-output
    cyclosis-extrinsic:fresh-line cyclosis-extrinsic:get-output-stream-string
    cyclosis-extrinsic:input-stream-p cyclosis-extrinsic:interactive-stream-p
    cyclosis-extrinsic:line-column cyclosis-extrinsic:line-length
    cyclosis-extrinsic:listen cyclosis-extrinsic:make-broadcast-stream
    cyclosis-extrinsic:make-concatenated-stream cyclosis-extrinsic:make-echo-stream
    cyclosis-extrinsic:make-string-input-stream cyclosis-extrinsic:make-string-output-stream
    cyclosis-extrinsic:make-synonym-stream cyclosis-extrinsic:make-two-way-stream
    cyclosis-extrinsic:open cyclosis-extrinsic:open-stream-p
    cyclosis-extrinsic:output-stream-p cyclosis-extrinsic:peek-char
    cyclosis-extrinsic:read-byte cyclosis-extrinsic:read-char
    cyclosis-extrinsic:read-char-no-hang cyclosis-extrinsic:read-line
    cyclosis-extrinsic:read-sequence cyclosis-extrinsic:start-line-p
    cyclosis-extrinsic:stream cyclosis-extrinsic:stream-element-type
    cyclosis-extrinsic:stream-external-format cyclosis-extrinsic:streamp
    cyclosis-extrinsic:string-stream cyclosis-extrinsic:synonym-stream
    cyclosis-extrinsic:synonym-stream-symbol cyclosis-extrinsic:terpri
    cyclosis-extrinsic:two-way-stream  cyclosis-extrinsic:two-way-stream-input-stream
    cyclosis-extrinsic:two-way-stream-output-stream cyclosis-extrinsic:unread-char
    cyclosis-extrinsic:with-input-from-string cyclosis-extrinsic:with-open-file
    cyclosis-extrinsic:with-open-stream cyclosis-extrinsic:with-output-to-string
    cyclosis-extrinsic:write-byte cyclosis-extrinsic:write-char
    cyclosis-extrinsic:write-line cyclosis-extrinsic:write-sequence
    cyclosis-extrinsic:write-string cyclosis-extrinsic:y-or-n-p
    cyclosis-extrinsic:yes-or-no-p))

(defun test (&key exit)
  (let ((system (asdf:find-system :cyclosis-extrinsic/test)))
    (ansi-test-harness:ansi-test :directory (merge-pathnames
                                             (make-pathname
                                              :directory '(:relative
                                                           "dependencies"
                                                           "ansi-test"))
                                             (asdf:component-pathname system))
                                 :tests *tests*
                                 :expected-failures (asdf:component-pathname
                                                     (asdf:find-component
                                                      system
                                                      '("code" "expected-failures.sexp")))
                                 :exit exit
                                 :extrinsic-symbols *symbols*)))
