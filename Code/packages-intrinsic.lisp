(cl:in-package #:common-lisp-user)

(defpackage #:cyclosis
  (:use #:common-lisp)
  (:export
   #:*terminal-io*
   #:*debug-io*
   #:*error-output*
   #:*query-io*
   #:*standard-input*
   #:*standard-output*
   #:*trace-output*
   ;; CL Streams classes.
   #:stream
   #:broadcast-stream
   #:concatenated-stream
   #:echo-stream
   #:file-stream
   #:string-stream
   #:synonym-stream
   #:two-way-stream
   ;; Gray Streams classes.
   #:fundamental-stream
   #:fundamental-input-stream
   #:fundamental-output-stream
   #:fundamental-binary-stream
   #:fundamental-character-stream
   #:fundamental-binary-input-stream
   #:fundamental-binary-output-stream
   #:fundamental-character-input-stream
   #:fundamental-character-output-stream
   ;; Utility Streams
   #:binary-output-stream
   #:binary-output-stream-buffer
   #:binary-output-stream-element-type
   ;; CL Stream functions
   #:input-stream-p
   #:output-stream-p
   #:interactive-stream-p
   #:open-stream-p
   #:stream-element-type
   #:streamp
   #:read-byte
   #:write-byte
   #:peek-char
   #:read-char
   #:read-char-no-hang
   #:terpri
   #:fresh-line
   #:unread-char
   #:write-char
   #:read-line
   #:write-string
   #:write-line
   #:read-sequence
   #:write-sequence
   #:file-length
   #:file-position
   #:file-string-length
   #:open
   #:stream-external-format
   #:close
   #:listen
   #:clear-input
   #:finish-output
   #:force-output
   #:clear-output
   #:y-or-n-p
   #:yes-or-no-p
   #:make-synonym-stream
   #:synonym-stream-symbol
   #:broadcast-stream-streams
   #:make-broadcast-stream
   #:make-two-way-stream
   #:two-way-stream-input-stream
   #:two-way-stream-output-stream
   #:echo-stream-input-stream
   #:echo-stream-output-stream
   #:make-echo-stream
   #:concatenated-stream-streams
   #:make-concatenated-stream
   #:get-output-stream-string
   #:make-string-input-stream
   #:make-string-output-stream
   #:stream-error-stream
   #:with-open-file
   #:with-open-stream
   #:with-input-from-string
   #:with-output-to-string
   ;; Methods common to all streams.
   #:stream-element-type
   #:close
   #:stream-file-position
   #:stream-file-length
   #:stream-file-string-length
   #:open-stream-p
   #:input-stream-p
   #:output-stream-p
   #:interactive-stream-p
   #:stream-external-format
   ;; Input stream methods.
   #:stream-clear-input
   #:stream-read-sequence
   ;; Output stream methods.
   #:stream-clear-output
   #:stream-finish-output
   #:stream-force-output
   #:stream-write-sequence
   ;;; Binary stream methods.
   #:stream-read-byte
   #:stream-read-byte-no-hang
   #:stream-write-byte
   #:stream-listen-byte
   ;;; Character input stream methods.
   #:stream-peek-char
   #:stream-read-char-no-hang
   #:stream-read-char
   #:stream-read-line
   #:stream-listen
   #:stream-unread-char
   ;;; Character output stream methods.
   #:stream-advance-to-column
   #:stream-fresh-line
   #:stream-line-column
   #:stream-line-length
   #:stream-start-line-p
   #:stream-terpri
   #:stream-write-char
   #:stream-write-string
   ;; Extensions.
   #:unread-char-mixin
   #:stream-display
   #:stream-peek-char-skip-whitespace
   #:stream-open-p
   #:external-format-string-length))
