(cl:in-package #:common-lisp-user)

(defpackage #:cyclosis
  (:use #:common-lisp)
  (:shadow #:broadcast-stream
           #:broadcast-stream-streams
           #:close
           #:concatenated-stream
           #:concatenated-stream-streams
           #:echo-stream
           #:echo-stream-input-stream
           #:echo-stream-output-stream
           #:file-stream
           #:get-output-stream-string
           #:input-stream-p
           #:interactive-stream-p
           #:make-broadcast-stream
           #:make-concatenated-stream
           #:make-echo-stream
           #:make-string-input-stream
           #:make-string-output-stream
           #:make-synonym-stream
           #:make-two-way-stream
           #:open-stream-p
           #:output-stream-p
           #:pathname
           #:stream
           #:stream-element-type
           #:stream-external-format
           #:streamp
           #:string-stream
           #:synonym-stream
           #:synonym-stream-symbol
           #:truename
           #:two-way-stream
           #:two-way-stream-input-stream
           #:two-way-stream-output-stream)
  (:export #:broadcast-stream
           #:broadcast-stream-streams
           #:close
           #:coerce-input-stream
           #:coerce-output-stream
           #:concatenated-stream
           #:concatenated-stream-streams
           #:define-interface
           #:echo-stream
           #:echo-stream-input-stream
           #:echo-stream-output-stream
           #:expand-with-input-from-string
           #:expand-with-open-file
           #:expand-with-open-stream
           #:expand-with-output-to-string
           #:file-stream
           #:fundamental-binary-input-stream
           #:fundamental-binary-output-stream
           #:fundamental-binary-stream
           #:fundamental-character-input-stream
           #:fundamental-character-output-stream
           #:fundamental-character-stream
           #:fundamental-input-stream
           #:fundamental-output-stream
           #:fundamental-stream
           #:get-output-stream-string
           #:input-stream-p
           #:interactive-stream-p
           #:make-broadcast-stream
           #:make-concatenated-stream
           #:make-echo-stream
           #:make-file-stream
           #:make-string-input-stream
           #:make-string-output-stream
           #:make-synonym-stream
           #:make-two-way-stream
           #:open-stream-p
           #:output-stream-p
           #:pathname
           #:state-value
           #:stream
           #:stream-advance-to-column
           #:stream-clear-input
           #:stream-clear-output
           #:stream-element-type
           #:stream-external-format
           #:stream-file-length
           #:stream-file-position
           #:stream-file-string-length
           #:stream-finish-output
           #:stream-force-output
           #:stream-fresh-line
           #:stream-line-column
           #:stream-line-length
           #:stream-line-number
           #:stream-listen
           #:stream-peek-char
           #:stream-read-byte
           #:stream-read-char
           #:stream-read-char-no-hang
           #:stream-read-line
           #:stream-read-sequence
           #:stream-start-line-p
           #:stream-terpri
           #:stream-unread-char
           #:stream-write-byte
           #:stream-write-char
           #:stream-write-sequence
           #:stream-write-string
           #:streamp
           #:string-stream
           #:synonym-stream
           #:synonym-stream-symbol
           #:truename
           #:two-way-stream
           #:two-way-stream-input-stream
           #:two-way-stream-output-stream
           #:whitespace-char-p))
