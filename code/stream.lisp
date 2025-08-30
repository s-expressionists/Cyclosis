(in-package #:cyclosis)

#+sbcl (require :sb-posix)

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

(defun open
    (client filespec direction element-type  if-exists if-exists-p if-does-not-exist
     if-does-not-exist-p external-format)
  (check-type direction (member :input :output :io :probe))
  (check-type if-exists (member :error :new-version :rename :rename-and-delete
                                :overwrite :append :supersede nil))
  (check-type if-does-not-exist (member :error :create nil))
  (let ((path (translate-logical-pathname (merge-pathnames (pathname filespec)))))
    (when (wild-pathname-p path)
      (error 'file-error :pathname path))
    (unless if-exists-p
      (setf if-exists (if (eql (pathname-version path) :newest)
                          :new-version
                          :error)))
    (when (eq direction :probe)
      (setf if-exists nil))
    (unless if-does-not-exist-p
      (cond ((or (eql direction :input)
                 (eql if-exists :overwrite)
                 (eql if-exists :append))
             (setf if-does-not-exist :error))
            ((or (eql direction :output)
                 (eql direction :io))
             (setf if-does-not-exist :create))
            ((eql direction :probe)
             (setf if-does-not-exist nil))))
    (make-file-stream client path direction
                      if-exists if-does-not-exist
                      element-type external-format)))

(defun peek-char (client peek-type stream eof-error-p eof-value recursive-p)
  (declare (ignore recursive-p))
  (check-type peek-type (or (eql t) (eql nil) character))
  (prog ((s (coerce-input-stream client stream))
         ch)
   repeat
     (setf ch (stream-peek-char s))
     (when (eq ch :eof)
       (when eof-error-p
         (error 'end-of-file :stream s))
       (return eof-value))
     (when (and peek-type
                (or (and (eq peek-type t)
                         (whitespace-char-p client ch))
                    (and (not (eq peek-type t))
                         (char/= peek-type ch))))
       (stream-read-char s)
       (go repeat))
     (return ch)))

(defun y-or-n-p (client control arguments)
  (let ((query-io (trinsic:cell-value client 'cl:*query-io* 'cl:variable)))
    (when control
      (stream-fresh-line query-io)
      (apply (trinsic:cell-value client 'cl:format 'cl:function)
             query-io control arguments)
      (stream-write-char query-io #\Space))
    (stream-write-string query-io "(Y or N) ")
    (stream-finish-output query-io)
    (loop
      (stream-clear-input query-io)
      (let ((c (stream-read-char query-io)))
        (when (char-equal c #\Y)
          (return t))
        (when (char-equal c #\N)
          (return nil)))
      (stream-fresh-line query-io)
      (stream-write-string query-io "Please respond with \"y\" or \"n\". "))))

(defun y-or-n-p/compiler-macro (client whole control arguments)
  (if (stringp control)
      `(y-or-n-p ,(trinsic:client-form client)
                 ,(funcall (trinsic:cell-value client 'cl:formatter 'cl:function) control)
                 ,@arguments)
      whole))

(defun yes-or-no-p (client control arguments)
  (let ((query-io (trinsic:cell-value client 'cl:*query-io* 'cl:variable)))
    (when control
      (stream-fresh-line query-io)
      (apply (trinsic:cell-value client 'cl:format 'cl:function)
             query-io control arguments)
      (stream-write-char query-io #\Space))
    (stream-write-string query-io "(Yes or No) ")
    (stream-finish-output query-io)
    (loop
      (stream-clear-input query-io)
      (let ((line (stream-read-line query-io)))
        (when (string-equal line "yes")
          (return t))
        (when (string-equal line "no")
          (return nil)))
      (stream-fresh-line query-io)
      (stream-write-string query-io "Please respond with \"yes\" or \"no\". ")
      (stream-finish-output query-io))))

(defun yes-or-no-p/compiler-macro (client whole control arguments)
  (if (stringp control)
      `(yes-or-no-p ,(trinsic:client-form client)
                    ,(funcall (trinsic:cell-value client 'cl:formatter 'cl:function) control)
                    ,@arguments)
      whole))

(defclass stream-input-stream-mixin ()
  ((%input-stream :accessor stream-input-stream
                  :initarg :input-stream
                  :type input-stream)))

(defmethod initialize-instance :after
    ((instance stream-input-stream-mixin) &rest initargs &key)
  (declare (ignore initargs))
  (check-input-stream (stream-input-stream instance)))

(defclass stream-output-stream-mixin ()
  ((%output-stream :accessor stream-output-stream
                   :initarg :output-stream
                   :type output-stream)))

(defmethod initialize-instance :after
    ((instance stream-output-stream-mixin) &rest initargs &key)
  (declare (ignore initargs))
  (check-output-stream (stream-output-stream instance)))

(defclass stream-input-streams-mixin ()
  ((%input-streams :accessor stream-input-streams
                   :initarg :input-streams
                   :type list)))

(defmethod initialize-instance :after
    ((instance stream-input-streams-mixin) &rest initargs &key)
  (declare (ignore initargs))
  (map nil #'check-input-stream (stream-input-streams instance)))

(defclass stream-output-streams-mixin ()
  ((%output-streams :accessor stream-output-streams
                    :initarg :output-streams
                    :type list)))

(defmethod initialize-instance :after
    ((instance stream-output-streams-mixin) &rest initargs &key)
  (declare (ignore initargs))
  (map nil #'check-output-stream (stream-output-streams instance)))

(defclass stream-symbol-mixin ()
  ((%symbol :accessor stream-symbol
            :initarg :symbol
            :type symbol)))

(defmethod initialize-instance :after ((instance stream-symbol-mixin) &rest initargs &key)
  (declare (ignore initargs))
  (check-type (stream-symbol instance) symbol))
