(cl:in-package #:common-lisp-user)

(defpackage #:cyclosis
  (:use #:common-lisp)
  (:export
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
