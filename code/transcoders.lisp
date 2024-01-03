(cl:in-package #:cyclosis)

(define-condition transcode-error (simple-error)
  ())

;;; utf-8 transcoder

(defclass utf-8-transcoder () ())

(defun utf-8-eof ()
  (error 'transcode-error
         :format-control "EOF before end of UTF-8 sequence."))

(defun utf-8-illegal-sequence ()
  (error 'transcode-error
         :format-control "Illegal UTF-8 sequence."))

(defmethod read-element ((transcoder utf-8-transcoder) stream)
  (let (octet1 octet2 octet3 octet4)
    (cond ((eq (setf octet1 (stream-read-octet stream)) :eof)
           :eof)
          ((< octet1 #b10000000)
           (code-char octet1))
          ((eq (setf octet2 (stream-read-octet input-stream)) :eof)
           (stream-unread-octet octet1)
           (utf-8-eof))
          ((not (<= #b10000000 octet2 #b10111111))
           (stream-unread-octet octet2)
           (stream-unread-octet octet1)
           (utf-8-illegal-sequence))
          ((< octet1 #b11100000)
           (code-char (+ (ash (- octet1 #b11000000) -2)
                         (- octet2 #b10000000))))
          ((eq (setf octet3 (stream-read-octet input-stream)) :eof)
           (stream-unread-octet octet2)
           (stream-unread-octet octet1)
           (utf-8-eof))
          ((not (<= #b10000000 octet3 #b10111111))
           (stream-unread-octet octet3)
           (stream-unread-octet octet2)
           (stream-unread-octet octet1)
           (utf-8-illegal-sequence))
          ((< octet1 #b11110000)
           (code-char (+ (ash (- octet1 #b11100000) -4)
                         (ash (- octet2 #b10000000) -2)
                         (- octet3 #b10000000))))
          ((eq (setf octet4 (stream-read-octet input-stream)) :eof)
           (stream-unread-octet octet3)
           (stream-unread-octet octet2)
           (stream-unread-octet octet1)
           (utf-8-eof))
          ((not (and (<= #b10000000 octet4 #b10111111)
                     (< octet1 #b11111000)))
           (stream-unread-octet octet4)
           (stream-unread-octet octet3)
           (stream-unread-octet octet2)
           (stream-unread-octet octet1)
           (utf-8-illegal-sequence))
          (t
           (code-char (+ (ash (- octet1 #b11110000) -6)
                         (ash (- octet2 #b10000000) -4)
                         (ash (- octet3 #b10000000) -2)
                         (- octet4 #b10000000)))))))

(defmethod write-element ((transcoder utf-8-transcoder) stream element)
  (let ((code (char-code element))
        (m1 #b10000000)
        (m2 #b00111111))
    (cond ((< code #x80)
           (stream-write-octet stream code))
          ((< code #x800)
           (stream-write-octet
            stream
            (logior #b11000000 (ash code -6)))
           (stream-write-octet stream (logior m1 (logand code m2))))
          ((< code #x1000)
           (stream-write-octet stream (logior #b11100000 (ash code -12)))
           (stream-write-octet stream (logior m1 (logand (ash code -6) m2)))
           (stream-write-octet stream (logior m1 (logand code m2))))
          (t
           (stream-write-octet stream (logior #b11110000 (ash code -18)))
           (stream-write-octet stream (logior m1 (logand (ash code -12) m2)))
           (stream-write-octet stream (logior m1 (logand (ash code -6) m2)))
           (stream-write-octet stream (logior m1 (logand code m2)))))))

(defun make-utf-8-transcoder (element-type external-format)
  (when (eq external-format :utf-8)
    (values (make-instance 'utf-8-transcoder) 'character '(:utf-8 :lf))))

(pushnew #'make-utf-8-transcoder *octet-transcoders*)

;;; unsigned-byte-8 transcoder

(defclass unsigned-byte-8-transcoder () ())

(defmethod read-element ((transcoder unsigned-byte-8-transcoder) stream)
  (stream-read-octet stream))

(defmethod write-element ((transcoder unsigned-byte-8-transcoder) stream element)
  (stream-write-octet stream element))

(defun make-unsigned-byte-8-transcoder (element-type external-format)
  (when (eq external-format :unsigned-byte-8)
    (values (make-instance 'unsigned-byte-8-transcoder) 'character :unsigned-byte-8)))

(pushnew #'make-unsigned-byte-8-transcoder *octet-transcoders*)
