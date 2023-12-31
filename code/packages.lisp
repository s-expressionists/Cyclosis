(cl:in-package #:common-lisp-user)

(defpackage #:cyclosis
  (:use #:common-lisp)
  (:shadow #:broadcast-stream
           #:broadcast-stream-streams
           #:clear-input
           #:clear-output
           #:close
           #:concatenated-stream
           #:concatenated-stream-streams
           #:echo-stream
           #:echo-stream-input-stream
           #:echo-stream-output-stream
           #:file-length
           #:file-position
           #:file-stream
           #:file-string-length
           #:finish-output
           #:force-output
           #:fresh-line
           #:get-output-stream-string
           #:input-stream-p
           #:interactive-stream-p
           #:listen
           #:make-broadcast-stream
           #:make-concatenated-stream
           #:make-echo-stream
           #:make-string-input-stream
           #:make-string-output-stream
           #:make-synonym-stream
           #:make-two-way-stream
           #:open
           #:open-stream-p
           #:output-stream-p
           #:peek-char
           #:read-byte
           #:read-char
           #:read-char-no-hang
           #:read-line
           #:read-sequence
           #:stream
           #:stream-element-type
           #:stream-external-format
           #:streamp
           #:string-stream
           #:synonym-stream
           #:synonym-stream-symbol
           #:terpri
           #:two-way-stream
           #:two-way-stream-input-stream
           #:two-way-stream-output-stream
           #:unread-char
           #:with-input-from-string
           #:with-output-to-string
           #:write-byte
           #:write-char
           #:write-line
           #:write-sequence
           #:write-string
           #:y-or-n-p
           #:yes-or-no-p)
  (:export #:broadcast-stream
           #:broadcast-stream-streams
           #:clear-input
           #:clear-output
           #:close
           #:concatenated-stream
           #:concatenated-stream-streams
           #:echo-stream
           #:echo-stream-input-stream
           #:echo-stream-output-stream
           #:file-length
           #:file-position
           #:file-stream
           #:file-string-length
           #:finish-output
           #:force-output
           #:fresh-line
           #:get-output-stream-string
           #:input-stream-p
           #:interactive-stream-p
           #:listen
           #:expand-with-open-stream
           #:expand-with-open-file
           #:make-broadcast-stream
           #:make-concatenated-stream
           #:make-echo-stream
           #:make-string-input-stream
           #:make-string-output-stream
           #:make-synonym-stream
           #:make-two-way-stream
           #:open
           #:open-stream-p
           #:output-stream-p
           #:peek-char
           #:read-byte
           #:read-char
           #:read-char-no-hang
           #:read-line
           #:read-sequence
           #:stream
           #:stream-element-type
           #:stream-external-format
           #:streamp
           #:string-stream
           #:define-interface
           #:synonym-stream
           #:synonym-stream-symbol
           #:terpri
           #:two-way-stream
           #:two-way-stream-input-stream
           #:two-way-stream-output-stream
           #:unread-char
           #:with-input-from-string
           #:with-output-to-string
           #:write-byte
           #:write-char
           #:write-line
           #:write-sequence
           #:write-string
           #:y-or-n-p
           #:yes-or-no-p))
