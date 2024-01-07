(in-package #:cyclosis)

(define-condition file-exists (file-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "The file ~S already exists"
             (file-error-pathname condition)))))

(define-condition file-does-not-exist (file-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "The file ~S does not exist"
             (file-error-pathname condition)))))

(define-condition open-fail (file-error)
  ((message :reader open-fail-message
            :initarg :message))
  (:report
   (lambda (condition stream)
     (format stream "Opening the file ~S failed: ~a"
             (file-error-pathname condition)
             (open-fail-message condition)))))

(define-condition transcode-error (stream-error)
  ((octets :reader transcode-error-reader
           :initform nil
           :initarg :octets)))

(define-condition illegal-sequence (transcode-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Illegal sequence ~s while decoding from ~s"
                     (transcode-error-octets condition)
                     (stream-error-stream condition)))))

(define-condition unexpected-eof (transcode-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Unexpected EOF in sequence ~s while decoding from ~s"
                     (transcode-error-octets condition)
                     (stream-error-stream condition)))))
