(cl:in-package #:common-lisp-user)

(defpackage #:cyclosis
  (:use #:common-lisp)
  (:export 
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
   #:stream-open-p
   #:external-format-string-length))
