(in-package #:cyclosis)

;; TWB: This came from Mezzano. This isn't implemented like this in
;; other implementations, so I am removing them for now.
#+(or)(defun listen-byte (&optional input-stream)
  ;; Note: Unlike STREAM-LISTEN, STREAM-LISTEN-BYTE may return :EOF
  ;; to indicate that the stream is at EOF. This should be equivalent to NIL.
  ;; It is used in the default implementation of STREAM-READ-BYTE-NO-HANG.
  (let ((result (stream-listen-byte (coerce-input-stream ,client-var input-stream))))
    (cond ((or (eql result :eof)
               (not result))
           nil)
          (t))))

;; TWB: This came from Mezzano. This isn't implemented like this in
;; other implementations, so I am removing them for now.
#+(or)(defun read-byte-no-hang (stream &optional (eof-error-p t) eof-value)
  (let* ((s (coerce-input-stream ,client-var stream))
         (b (stream-read-byte-no-hang s)))
    (check-type b (or integer (eql :eof) null))
    (cond ((eql b :eof)
           (when eof-error-p
             (error 'end-of-file :stream s))
           eof-value)
          (b))))

;;; Macro expansion

(defun expand-with-open-stream (var stream body)
  (multiple-value-bind (body-forms declares)
      (alexandria:parse-body body)
    `(let ((,var ,stream))
       ,@declares
       (unwind-protect
            (progn ,@body-forms)
         (close ,var)))))

(defun expand-with-open-file (open-sym var filespec options body)
  (multiple-value-bind (body-forms declares)
      (alexandria:parse-body body)
    (let ((abortp (gensym "ABORTP")))
      `(let ((,var (,open-sym ,filespec ,@options))
             (,abortp t))
         ,@declares
         (unwind-protect
              (multiple-value-prog1
                  (progn ,@body-forms)
                (setf ,abortp nil))
           (when ,var
             (close ,var :abort ,abortp)))))))

(defclass binary-output-stream (fundamental-binary-output-stream)
  ((element-type :initarg :element-type
                 :reader binary-output-stream-element-type)
   (buffer :initarg :buffer
           :initform nil
           :accessor binary-output-stream-buffer))
  (:default-initargs :element-type '(unsigned-byte 8)))

(defmethod initialize-instance :after
    ((instance binary-output-stream) &rest initargs &key &allow-other-keys)
 (declare (ignore initargs))
 (unless (binary-output-stream-buffer instance)
   (setf (binary-output-stream-buffer instance)
        (make-array 8 :element-type (binary-output-stream-element-type instance)
                    :adjustable t :fill-pointer 0))))

(defun make-binary-output-stream (&key (element-type '(unsigned-byte 8)) (buffer nil bufferp))
  (when bufferp
    (when (not (and (vectorp buffer)
                    (array-has-fill-pointer-p buffer)))
      (error "~S must be a vector with a fill-pointer" buffer)))
  (when (not (subtypep element-type 'integer))
    (error "Element-type ~S must be a subtype of INTEGER" element-type))
  (make-instance 'binary-output-stream :element-type element-type :buffer buffer))

(defmethod stream-write-byte ((stream binary-output-stream) integer)
  (vector-push-extend integer (binary-output-stream-buffer stream)))

(defmethod stream-write-sequence ((stream binary-output-stream) seq &optional (start 0) end)
  (setf end (or end (length seq)))
  (let ((n-bytes (- end start)))
    (let* ((output (binary-output-stream-buffer stream))
           (current-length (length output))
           (new-length (+ (length output) n-bytes)))
      (when (< (array-dimension output 0) new-length)
        (adjust-array output new-length))
      (setf (fill-pointer output) new-length)
      (replace output seq
               :start1 current-length
               :start2 start
               :end2 end)
      seq)))