(cl:in-package #:cyclosis)

(define-octet-transcoder ascii-transcoder :ascii
  (#x00 #x7f #x00 #x7f))
