(in-package #:cyclosis)

#+(or)(defgeneric stream-with-edit (stream fn))
#+(or)(defgeneric stream-cursor-pos (stream))
#+(or)(defgeneric stream-character-width (stream character))
#+(or)(defgeneric stream-compute-motion (stream string &optional start end initial-x initial-y))
#+(or)(defgeneric stream-clear-between (stream start-x start-y end-x end-y))
#+(or)(defgeneric stream-move-to (stream x y))

(defun ensure-symbol (name &optional (package *package*))
  (intern (string name) package))

(defgeneric coerce-input-stream (client designator))

(defmethod cyclosis:coerce-input-stream (client designator)
  (declare (ignore client))
  (unless (input-stream-p designator)
    (error 'type-error :datum designator :expected-type 'stream))
  (unless (open-stream-p designator)
    (error 'stream-error :stream designator))
  designator)

(defgeneric coerce-output-stream (client designator))

(defmethod cyclosis:coerce-output-stream (client designator)
  (declare (ignore client))
  (unless (output-stream-p designator)
    (error 'type-error :datum designator :expected-type 'stream))
  (unless (open-stream-p designator)
    (error 'stream-error :stream designator))
  designator)

(defgeneric make-file-stream (client path direction if-exists if-does-not-exist element-type external-format))

(defgeneric make-code (client element-type external-format))

(defgeneric encode (code value octets))

(defgeneric decode (code value octets))

(defgeneric encoded-length (code value))

(defgeneric decoded-length (code octets))

(defgeneric write-octets (stream octets))

(defgeneric read-octets (stream octets))

(defgeneric stream-code (stream))

(defgeneric (setf stream-code) (new-value stream))

(defclass octet-stream-mixin ()
  ((code :accessor stream-code
         :initarg :code)
   (element-type :accessor stream-element-type
                 :initarg :element-type)
   (external-format :accessor stream-external-format
                    :initarg :external-format)))

(defmacro define-interface (client-var &key intrinsic)
  (let* ((intrinsic-pkg (if intrinsic (find-package '#:common-lisp) *package*))
         (open-sym (ensure-symbol '#:open intrinsic-pkg)))
    `(progn
       (defun ,open-sym
           (filespec
            &key (direction :input)
                 (element-type 'character)
                 (if-exists nil if-exists-p)
                 (if-does-not-exist nil if-does-not-exist-p)
                 (external-format :default))
         (check-type direction (member :input :output :io :probe))
         (check-type if-exists (member :error :new-version :rename :rename-and-delete
                                       :overwrite :append :supersede nil))
         (check-type if-does-not-exist (member :error :create nil))
         (let ((path (translate-logical-pathname (merge-pathnames filespec))))
           (when (wild-pathname-p path)
             (error "Wild pathname specified."))
           (unless if-exists-p
             (setf if-exists (if (eql (pathname-version path) :newest)
                                 :new-version
                                 :error)))
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
           (make-file-stream ,client-var path direction
                             if-exists if-does-not-exist
                             element-type external-format)))

       (defmacro ,(ensure-symbol '#:with-open-stream intrinsic-pkg)
           ((var stream) &body body)
         (expand-with-open-stream var stream body))

       (defmacro ,(ensure-symbol '#:with-open-file intrinsic-pkg)
           ((var filespec &rest options) &body body)
         (expand-with-open-file ',open-sym var filespec options body))

       (defmacro ,(ensure-symbol '#:with-input-from-string intrinsic-pkg)
           ((var string &key index start end) &body body)
         (expand-with-input-from-string var string start end index body))

       (defmacro ,(ensure-symbol '#:with-output-to-string intrinsic-pkg)
           ((var &optional string-form &key element-type) &body body)
         (expand-with-output-to-string var string-form element-type body))

       (defun ,(ensure-symbol '#:read-byte intrinsic-pkg)
           (stream &optional (eof-error-p t) eof-value)
         (let ((b (stream-read-byte (coerce-input-stream ,client-var stream))))
           (check-type b (or integer (eql :eof)))
           (if (eql b :eof)
               (if eof-error-p
                   (error 'end-of-file :stream stream)
                   eof-value)
               b)))

       (defun ,(ensure-symbol '#:write-byte intrinsic-pkg)
           (byte stream)
         (stream-write-byte (coerce-output-stream ,client-var stream) byte))

       (defun ,(ensure-symbol '#:read-sequence intrinsic-pkg)
           (sequence stream &key (start 0) end)
         (stream-read-sequence (coerce-input-stream ,client-var stream) sequence
                               start end))

       (defun ,(ensure-symbol '#:write-sequence intrinsic-pkg)
           (sequence stream &key (start 0) end)
         (stream-write-sequence (coerce-output-stream ,client-var stream) sequence
                                start end)
         sequence)

       (defun ,(ensure-symbol '#:file-position intrinsic-pkg)
           (stream &optional (position-spec nil position-spec-p))
         (check-type stream stream)
         (cond (position-spec-p
                (check-type position-spec (or (integer 0) (member :start :end)))
                (stream-file-position stream position-spec))
               (t
                (stream-file-position stream))))

       (defun ,(ensure-symbol '#:file-length intrinsic-pkg)
           (stream)
         (check-type stream stream)
         (stream-file-length stream))

       (defun ,(ensure-symbol '#:file-string-length intrinsic-pkg)
           (stream object)
         (check-type stream stream)
         (check-type object (or string character))
         (when (characterp object)
           (setf object (string object)))
         (stream-file-string-length stream object))

       (defun ,(ensure-symbol '#:read-char intrinsic-pkg)
           (&optional stream (eof-error-p t) eof-value recursive-p)
         (declare (ignore recursive-p))
         (let* ((s (coerce-input-stream ,client-var stream))
                (c (stream-read-char s)))
           (check-type c (or character (eql :eof)))
           (cond ((eql c :eof)
                  (when eof-error-p
                    (error 'end-of-file :stream s))
                  eof-value)
                 (c))))

       (defun ,(ensure-symbol '#:read-char-no-hang intrinsic-pkg)
           (&optional stream (eof-error-p t) eof-value recursive-p)
         (declare (ignore recursive-p))
         (let* ((s (coerce-input-stream ,client-var stream))
                (c (stream-read-char-no-hang s)))
           (check-type c (or character (eql :eof) null))
           (cond ((eql c :eof)
                  (when eof-error-p
                    (error 'end-of-file :stream s))
                  eof-value)
                 (c))))

       (defun ,(ensure-symbol '#:read-line intrinsic-pkg)
           (&optional input-stream (eof-error-p t) eof-value recursive-p)
         (declare (ignore recursive-p))
         (setf input-stream (coerce-input-stream ,client-var input-stream))
         (multiple-value-bind (line missing-newline-p)
             (stream-read-line input-stream)
           (if (and (zerop (length line))
                    missing-newline-p)
               (if eof-error-p
                   (error 'end-of-file :stream input-stream)
                   (values eof-value t))
               (values line missing-newline-p))))

       (defun ,(ensure-symbol '#:unread-char intrinsic-pkg)
           (character &optional stream)
         (let ((s (coerce-input-stream ,client-var stream)))
           (check-type character character)
           (stream-unread-char s character)
           nil))

       (defun ,(ensure-symbol '#:peek-char intrinsic-pkg)
           (&optional peek-type stream (eof-error-p t) eof-value recursive-p)
         (check-type peek-type (or (eql t) (eql nil) character))
         (let ((s (coerce-input-stream ,client-var stream)))
           (if (characterp peek-type)
               (loop for ch = (read-char s eof-error-p nil recursive-p)
                     until (or (not ch)
                               (char= ch peek-type))
                     finally (return (cond (ch
                                            (unread-char ch s)
                                            ch)
                                           (t
                                            eof-value))))
               (let ((ch (if peek-type
                             (stream-peek-char-skip-whitespace s)
                             (stream-peek-char s))))
                 (cond ((not (eql ch :eof)) ch)
                       (eof-error-p
                        (error 'end-of-file :stream s))
                       (t eof-value))))))

       (defun ,(ensure-symbol '#:clear-input intrinsic-pkg)
           (&optional stream)
         (stream-clear-input (coerce-input-stream ,client-var stream))
         nil)

       (defun ,(ensure-symbol '#:finish-output intrinsic-pkg)
           (&optional output-stream)
         (stream-finish-output (coerce-output-stream ,client-var output-stream))
         nil)

       (defun ,(ensure-symbol '#:force-output intrinsic-pkg)
           (&optional output-stream)
         (stream-force-output (coerce-output-stream ,client-var output-stream))
         nil)

       (defun ,(ensure-symbol '#:clear-output intrinsic-pkg)
           (&optional output-stream)
         (stream-clear-output (coerce-output-stream ,client-var output-stream))
         nil)

       (defun ,(ensure-symbol '#:write-char intrinsic-pkg)
           (character &optional stream)
         (let ((s (coerce-output-stream ,client-var stream)))
           (check-type character character)
           (stream-write-char s character)
           character))

       (defun,(ensure-symbol '#:listen intrinsic-pkg)
           (&optional input-stream)
         (stream-listen (coerce-input-stream ,client-var input-stream)))

       (defun ,(ensure-symbol '#:y-or-n-p intrinsic-pkg)
           (&optional control &rest arguments)
         (declare (dynamic-extent arguments))
         (when control
           (fresh-line *query-io*)
           (apply 'format *query-io* control arguments)
           (write-char #\Space *query-io*))
         (format *query-io* "(Y or N) ")
         (loop
           (clear-input *query-io*)
           (let ((c (read-char *query-io*)))
             (when (char-equal c #\Y)
               (return t))
             (when (char-equal c #\N)
               (return nil)))
           (fresh-line *query-io*)
           (format *query-io* "Please respond with \"y\" or \"n\". ")))

       (defun ,(ensure-symbol '#:yes-or-no-p intrinsic-pkg)
           (&optional control &rest arguments)
         (declare (dynamic-extent arguments))
         (when control
           (fresh-line *query-io*)
           (apply 'format *query-io* control arguments)
           (write-char #\Space *query-io*))
         (format *query-io* "(Yes or No) ")
         (loop
           (clear-input *query-io*)
           (let ((line (read-line *query-io*)))
             (when (string-equal line "yes")
               (return t))
             (when (string-equal line "no")
               (return nil)))
           (fresh-line *query-io*)
           (format *query-io* "Please respond with \"yes\" or \"no\". ")))

       (defun ,(ensure-symbol '#:write-string intrinsic-pkg)
           (string &optional stream &key (start 0) end)
         (check-type string string)
         (stream-write-string (coerce-output-stream ,client-var stream) string start end)
         string)

       (defun ,(ensure-symbol '#:write-line intrinsic-pkg)
           (string &optional stream &key (start 0) end)
         (check-type string string)
         (let ((stream (coerce-output-stream ,client-var stream)))
           (stream-write-string stream string start end)
           (stream-terpri stream))
         string)

       (defun ,(ensure-symbol '#:terpri intrinsic-pkg)
           (&optional stream)
         (stream-terpri (coerce-output-stream ,client-var stream))
         nil)

       (defun ,(ensure-symbol '#:fresh-line intrinsic-pkg)
           (&optional stream)
         (stream-fresh-line (coerce-output-stream ,client-var stream)))

       (defun ,(ensure-symbol '#:start-line-p)
           (&optional stream)
         (stream-start-line-p (coerce-output-stream ,client-var stream)))

       (defun ,(ensure-symbol '#:line-column)
           (&optional stream)
         (stream-line-column (coerce-output-stream ,client-var stream)))

       (defun ,(ensure-symbol '#:line-length)
           (&optional stream)
         (stream-line-length (coerce-output-stream ,client-var stream)))

       (defun ,(ensure-symbol '#:advance-to-column)
           (column &optional stream)
         (stream-advance-to-column (coerce-output-stream ,client-var stream) column)))))

#+(or)(defun listen-byte (&optional input-stream)
  ;; Note: Unlike STREAM-LISTEN, STREAM-LISTEN-BYTE may return :EOF
  ;; to indicate that the stream is at EOF. This should be equivalent to NIL.
  ;; It is used in the default implementation of STREAM-READ-BYTE-NO-HANG.
  (let ((result (stream-listen-byte (coerce-input-stream ,client-var input-stream))))
    (cond ((or (eql result :eof)
               (not result))
           nil)
          (t))))

#+(or)(defun read-byte-no-hang (stream &optional (eof-error-p t) eof-value)
  (let* ((s (coerce-input-stream ,client-var stream))
         (b (stream-read-byte-no-hang s)))
    (check-type b (or integer (eql :eof) null))
    (cond ((eql b :eof)
           (when eof-error-p
             (error 'end-of-file :stream s))
           eof-value)
          (b))))

#+(or)(defmacro with-stream-editor ((stream recursive-p) &body body)
  "Activate the stream editor functionality for STREAM."
  `(%with-stream-editor ,stream ,recursive-p (lambda () (progn ,@body))))

#+(or)(defun %with-stream-editor (stream recursive-p fn)
  (cond ((typep stream 'synonym-stream)
         (%with-stream-editor (symbol-value (synonym-stream-symbol stream)) recursive-p fn))
        (recursive-p
         (funcall fn))
        (t (stream-with-edit stream fn))))

#+(or)(defmethod stream-with-edit ((stream stream) fn)
  (funcall fn))

#+(or)(defclass case-correcting-stream (fundamental-character-output-stream)
  ((stream :initarg :stream)
   (case :initarg :case)
   (position :initform :initial))
  (:documentation "Convert all output to the specified case.
CASE may be one of:
:UPCASE - Convert to uppercase.
:DOWNCASE - Convert to lowercase.
:INVERT - Invert the case.
:TITLECASE - Capitalise the start of each word, downcase the remaining letters.
:SENTENCECASE - Capitalise the start of the first word."))

#+(or)(defun make-case-correcting-stream (stream case)
  (make-instance 'case-correcting-stream
                 :stream stream
                 :case case))

#+(or)(defun case-correcting-write (character stream)
  (ecase (slot-value stream 'case)
    (:upcase (write-char (char-upcase character) (slot-value stream 'stream)))
    (:downcase (write-char (char-downcase character) (slot-value stream 'stream)))
    (:invert (write-char (if (upper-case-p character)
                             (char-downcase character)
                             (char-upcase character))
                         (slot-value stream 'stream)))
    (:titlecase
     (ecase (slot-value stream 'position)
       ((:initial :after-word)
        (if (alphanumericp character)
            (progn
              (setf (slot-value stream 'position) :mid-word)
              (write-char (char-upcase character) (slot-value stream 'stream)))
            (write-char character (slot-value stream 'stream))))
       (:mid-word
        (unless (alphanumericp character)
          (setf (slot-value stream 'position) :after-word))
        (write-char (char-downcase character) (slot-value stream 'stream)))))
    (:sentencecase
     (if (eql (slot-value stream 'position) :initial)
         (if (alphanumericp character)
             (progn
               (setf (slot-value stream 'position) nil)
               (write-char (char-upcase character) (slot-value stream 'stream)))
             (write-char character (slot-value stream 'stream)))
         (write-char (char-downcase character) (slot-value stream 'stream))))))

#+(or)(defmethod stream-write-char ((stream case-correcting-stream) character)
  (case-correcting-write character stream))

#+(or)(defclass simple-edit-mixin ()
  ((edit-buffer :initform nil)
   (edit-offset :initform nil)
   (edit-handler :initform nil)))

#+(or)(defmethod stream-read-char :around ((stream simple-edit-mixin))
  (let ((buffer (slot-value stream 'edit-buffer))
        (offset (slot-value stream 'edit-offset)))
    (if (and buffer (< offset (fill-pointer buffer)))
        (prog1 (aref buffer offset)
          (incf (slot-value stream 'edit-offset)))
        (do () (nil)
          (let ((ch (call-next-method)))
            (when ch
              (cond ((or (graphic-char-p ch) (eql #\Newline ch))
                     (when buffer
                       (vector-push-extend ch buffer)
                       (incf (slot-value stream 'edit-offset)))
                     (return (write-char ch stream)))
                    ((eql #\Backspace ch)
                     (when (slot-value stream 'edit-handler)
                       (funcall (slot-value stream 'edit-handler) ch))))))))))

#+(or)(defmethod stream-clear-input :before ((stream simple-edit-mixin))
  (when (slot-value stream 'edit-buffer)
    (setf (fill-pointer (slot-value stream 'edit-buffer)) 0
          (slot-value stream 'edit-offset) 0)))

#+(or)(defmethod stream-with-edit ((stream simple-edit-mixin) fn)
  (let ((old-buffer (slot-value stream 'edit-buffer))
        (old-offset (slot-value stream 'edit-offset))
        (old-handler (slot-value stream 'edit-handler))
        (buffer (make-array 100
                            :element-type 'character
                            :adjustable t
                            :fill-pointer 0)))
    (unwind-protect
         (multiple-value-bind (start-x start-y)
             (stream-cursor-pos stream)
           (setf (slot-value stream 'edit-buffer) buffer)
           (do () (nil)
            again
             (flet ((handler (ch)
                      (declare (ignore ch))
                      (when (> (fill-pointer buffer) 0)
                        (decf (fill-pointer buffer))
                        (multiple-value-bind (x y)
                            (stream-compute-motion stream
                                                   buffer
                                                   0 nil
                                                   start-x start-y)
                          (multiple-value-bind (cx cy) (stream-cursor-pos stream)
                            (stream-clear-between stream x y cx cy))
                          (stream-move-to stream x y)))
                      (go again)))
               (setf (slot-value stream 'edit-offset) 0
                     (slot-value stream 'edit-handler) #'handler)
               (return (funcall fn)))))
      (setf (slot-value stream 'edit-buffer) old-buffer
            (slot-value stream 'edit-offset) old-offset
            (slot-value stream 'edit-handler) old-handler))))

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
