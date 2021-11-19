(cl:in-package #:common-lisp-user)

(defpackage #:cyclosis
  (:use #:common-lisp)
  (:shadow
   #:broadcast-stream
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
   #:with-open-file
   #:with-open-stream
   #:with-output-to-string
   #:write-byte
   #:write-char
   #:write-line
   #:write-sequence
   #:write-string
   #:yes-or-no-p
   #:y-or-n-p)
  (:export
   ;;; CL symbols
   #:broadcast-stream
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
   #:with-open-file
   #:with-open-stream
   #:with-output-to-string
   #:write-byte
   #:write-char
   #:write-line
   #:write-sequence
   #:write-string
   #:yes-or-no-p
   #:y-or-n-p
   ;;; Interface symbols
   #:fundamental-stream
   #:fundamental-input-stream
   #:fundamental-output-stream
   #:fundamental-binary-stream
   #:fundamental-character-stream
   #:fundamental-binary-input-stream
   #:fundamental-binary-output-stream
   #:fundamental-character-input-stream
   #:fundamental-character-output-stream
   #:binary-output-stream
   #:binary-output-stream-buffer
   #:binary-output-stream-element-type
   #:stream-file-position
   #:stream-file-length
   #:stream-file-string-length
   #:stream-clear-input
   #:stream-read-sequence
   #:stream-clear-output
   #:stream-finish-output
   #:stream-force-output
   #:stream-write-sequence
   #:stream-read-byte
   #:stream-read-byte-no-hang
   #:stream-write-byte
   #:stream-listen-byte
   #:stream-peek-char
   #:stream-read-char-no-hang
   #:stream-read-char
   #:stream-read-line
   #:stream-listen
   #:stream-unread-char
   #:stream-advance-to-column
   #:stream-fresh-line
   #:stream-line-column
   #:stream-line-length
   #:stream-start-line-p
   #:stream-terpri
   #:stream-write-char
   #:stream-write-string
   #:unread-char-mixin
   #:stream-peek-char-skip-whitespace
   #:stream-open-p
   #:external-format-string-length))
