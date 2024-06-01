(in-package #:cyclosis)

;; TWB: This came from Mezzano. This isn't implemented like this in
;; other implementations, so I am removing them for now.
#+(or)(defun listen-byte (&optional input-stream)
  ;; Note: Unlike STREAM-LISTEN, STREAM-LISTEN-BYTE may return :EOF
  ;; to indicate that the stream is at EOF. This should be equivalent to NIL.
  ;; It is used in the default implementation of STREAM-READ-BYTE-NO-HANG.
  (let ((result (stream-listen-byte (coerce-input-stream client-var input-stream))))
    (cond ((or (eql result :eof)
               (not result))
           nil)
          (t))))

;; TWB: This came from Mezzano. This isn't implemented like this in
;; other implementations, so I am removing them for now.
#+(or)(defun read-byte-no-hang (stream &optional (eof-error-p t) eof-value)
  (let* ((s (coerce-input-stream client-var stream))
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

(defun make-cursor ()
  (let ((cursor (list (cons 0 1) (cons 0 1))))
    (setf (cddr cursor) cursor)
    cursor))

(defmacro update-cursor (cursor ch)
  (let ((ch-var (gensym)))
    `(let ((,ch-var ,ch))
       (cond ((not (characterp ,ch-var)))
             ((char= ,ch-var #\newline)
              (setf (caadr ,cursor) 0
                    (cdadr ,cursor) (1+ (cdar ,cursor)))
              (pop ,cursor))
             (t
              (setf (caadr ,cursor) (1+ (caar ,cursor))
                    (cdadr ,cursor) (cdar ,cursor))
              (pop ,cursor))))))

(defun reset-cursor (cursor)
  (setf (caar cursor) 0
        (cdar cursor) 1
        (caadr cursor) 0
        (cdadr cursor) 1))

;;; character-input-mixin

(defclass character-input-mixin ()
  ((previous-char :accessor previous-char
                  :initform nil)
   (input-cursor :accessor input-cursor
                 :initform (make-cursor)))
  (:documentation "Mixin to add simple UNREAD-CHAR support to a stream."))

(defmethod stream-read-char :around ((stream character-input-mixin))
  (with-accessors ((previous-char previous-char)
                   (input-cursor input-cursor))
      stream
    (let (ch)
      (if previous-char
          (setf ch previous-char
                previous-char nil)
          (setf ch (call-next-method)))
      (update-cursor input-cursor ch)
      ch)))

(defmethod stream-read-char-no-hang :around ((stream character-input-mixin))
  (with-accessors ((previous-char previous-char)
                   (input-cursor input-cursor))
      stream
    (let (ch)
      (if previous-char
          (setf ch previous-char
                previous-char nil)
          (setf ch (call-next-method)))
      (update-cursor input-cursor ch)
      ch)))

(defmethod stream-unread-char ((stream character-input-mixin) character)
  (with-accessors ((previous-char previous-char)
                   (input-cursor input-cursor))
      stream
    (when previous-char
      (error "Multiple UNREAD-CHAR"))
    (pop input-cursor)
    (setf previous-char character)))

(defmethod stream-listen :around ((stream character-input-mixin))
  (or (and (previous-char stream) t)
      (call-next-method)))

(defmethod stream-clear-input :before ((stream character-input-mixin))
  (setf (previous-char stream) nil))

(defmethod stream-input-column ((stream character-input-mixin))
  (caar (input-cursor stream)))

(defmethod stream-input-line ((stream character-input-mixin))
  (cdar (input-cursor stream)))

;;; character-output-mixin

(defclass character-output-mixin ()
  ((output-cursor :accessor output-cursor
                  :initform (make-cursor))
   (line-length :accessor stream-line-length
                :initform nil)))

(defun update-output-cursor (stream ch)
  (update-cursor (output-cursor stream) ch))

(defmethod stream-line-column ((stream character-output-mixin))
  (caar (output-cursor stream)))

(defmethod (setf stream-line-column) (new-value (stream character-output-mixin))
  (setf (caar (output-cursor stream)) new-value))

(defmethod stream-line-number ((stream character-output-mixin))
  (cdar (output-cursor stream)))

(defmethod (setf stream-line-number) (new-value (stream character-output-mixin))
  (setf (cdar (output-cursor stream)) new-value))

(defmethod stream-write-char :after ((stream character-output-mixin) ch)
  (update-cursor (output-cursor stream) ch))

;;; octet-mixin

(defclass octet-mixin ()
  ((transcoder :initarg :transcoder
               :accessor octet-transcoder)
   (element-type :initarg :element-type
                 :accessor stream-element-type
                 :accessor octet-element-type)
   (external-format :initarg :external-format
                    :initform :default
                    :accessor stream-external-format
                    :accessor octet-external-format)))

(defun update-transcoder (instance)
  (with-accessors ((transcoder octet-transcoder)
                   (element-type octet-element-type)
                   (external-format octet-external-format))
      instance
    (setf transcoder
          (apply #'make-transcoder
                 (cond ((listp external-format)
                        (car external-format))
                       ((not (eq external-format :default))
                        external-format)
                       ((subtypep element-type 'integer)
                        *default-binary-external-format*)
                       ((subtypep element-type 'character)
                        *default-character-external-format*)
                       (t
                        (error "Unknown element type ~s" element-type)))
                 element-type
                 (if (listp external-format)
                     (cdr external-format)
                     nil)))))

(defmethod initialize-instance :after ((instance octet-mixin) &rest initargs)
  (declare (ignore initargs))
  (update-transcoder instance))

(defmethod (setf stream-element-type) :after (new-value (stream octet-mixin))
  (declare (ignore new-value))
  (update-transcoder stream))

(defmethod (setf stream-external-format) :after (new-value (stream octet-mixin))
  (declare (ignore new-value))
  (update-transcoder stream))

(defmethod stream-read-octet ((stream octet-mixin))
  (let* ((octets (make-array 1 :element-type '(unsigned-byte 8)))
         (count (stream-read-octets stream octets)))
    (if (zerop count)
        :eof
        (aref octets 0))))

(defmethod stream-write-octet ((stream octet-mixin) octet)
  (let ((octets (make-array 1
                            :element-type '(unsigned-byte 8)
                            :initial-element octet)))
    (stream-write-octets stream octets)
    octet))

(defmethod stream-read-char ((stream octet-mixin))
  (check-character-stream stream)
  (read-element (octet-transcoder stream) stream))

(defmethod stream-write-char ((stream octet-mixin) char)
  (check-character-stream stream)
  (write-element (octet-transcoder stream) stream char))

(defmethod stream-read-byte ((stream octet-mixin))
  (check-binary-stream stream)
  (read-element (octet-transcoder stream) stream))

(defmethod stream-write-byte ((stream octet-mixin) integer)
  (check-binary-stream stream)
  (write-element (octet-transcoder stream) stream integer))

(defmethod stream-file-string-length ((stream octet-mixin) string)
  (encoded-length (octet-transcoder stream) string))
