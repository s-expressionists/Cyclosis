(cl:in-package #:cyclosis)

;;; The standard stream class.

(defclass stream () ())

(defclass file-stream (stream) ())

;;; Gray Streams classes.

(defclass fundamental-stream (stream)
  ((%openp :initform t :accessor stream-open-p))
  (:documentation "The base class for all Gray streams."))

(defclass fundamental-input-stream (fundamental-stream)
  ()
  (:documentation "A superclass of all Gray input streams."))

(defclass fundamental-output-stream (fundamental-stream)
  ()
  (:documentation "A superclass of all Gray output streams."))

(defclass fundamental-character-stream (fundamental-stream)
  ()
  (:documentation "A superclass of all Gray streams whose element-type is a subtype of character."))

(defclass fundamental-binary-stream (fundamental-stream)
  ()
  (:documentation "A superclass of all Gray streams whose element-type is a subtype of unsigned-byte or signed-byte."))

(defclass fundamental-character-input-stream (fundamental-input-stream fundamental-character-stream)
  ()
  (:documentation "A superclass of all Gray input streams whose element-type is a subtype of unsigned-byte or signed-byte."))

(defclass fundamental-character-output-stream (fundamental-output-stream fundamental-character-stream)
  ()
  (:documentation "A superclass of all Gray output streams whose element-type is a subtype of character."))

(defclass fundamental-binary-input-stream (fundamental-input-stream fundamental-binary-stream)
  ()
  (:documentation "A superclass of all Gray input streams whose element-type is a subtype of unsigned-byte or signed-byte."))

(defclass fundamental-binary-output-stream (fundamental-output-stream fundamental-binary-stream)
  ()
  (:documentation "A superclass of all Gray output streams whose element-type is a subtype of unsigned-byte or signed-byte."))

;;; Gray stream generic functions

;;; Character input

(defgeneric stream-read-char (stream))

(defgeneric stream-unread-char (stream character))

(defgeneric stream-read-char-no-hang (stream))

(defgeneric stream-peek-char (stream))

(defgeneric stream-listen (stream))

(defgeneric stream-read-line (stream))

(defgeneric stream-clear-input (stream))

;;; Character output

(defgeneric stream-write-char (stream character))

(defgeneric stream-line-column (stream))

(defgeneric (setf stream-line-column) (new-value stream))

(defgeneric stream-line-number (stream))

(defgeneric (setf stream-line-number) (new-value stream))

(defgeneric stream-start-line-p (stream))

(defgeneric stream-write-string (stream string &optional start end))

(defgeneric stream-terpri (stream))

(defgeneric stream-fresh-line (stream))

(defgeneric stream-finish-output (stream))

(defgeneric stream-force-output (stream))

(defgeneric stream-clear-output (stream))

(defgeneric stream-advance-to-column (stream column))

;;; Other functions

(defgeneric close (stream &key abort))

(defgeneric open-stream-p (stream))

(defgeneric streamp (stream))

(defgeneric input-stream-p (stream))

(defgeneric output-stream-p (stream))

(defgeneric stream-element-type (stream))

(defgeneric (setf stream-element-type) (new-value stream))

;;; Binary streams

(defgeneric stream-read-byte (stream))

(defgeneric stream-write-byte (stream integer))

;;; Extensions to Gray Streams

;;; Common Lisp functions made generic

(defgeneric stream-external-format (stream))

(defgeneric (setf stream-external-format) (new-value stream))

(defgeneric interactive-stream-p (stream))

;;; Generic support for other CL stream functions

(defgeneric stream-file-position (stream &optional position-spec))

(defgeneric stream-file-length (stream))

(defgeneric stream-file-string-length (stream string))

(defgeneric stream-read-sequence (stream seq &optional start end))

(defgeneric stream-write-sequence (stream seq &optional start end))

(defgeneric stream-line-length (stream))

(defgeneric (setf stream-line-length) (new-value stream))

(defgeneric stream-input-column (stream))

(defgeneric stream-input-line (stream))

(defgeneric pathname (stream))

(defgeneric truename (stream))

;;; Generalization for byte streams and other extensions

;;; TWB: These came from Mezzano. They aren't implemented like this in
;;; other implementation, so I am removing them for now.

#+(or)(defgeneric stream-read-byte-no-hang (stream))

#+(or)(defgeneric stream-listen-byte (stream))

#+(or)(defgeneric stream-peek-char-skip-whitespace (stream))

(defgeneric whitespace-char-p (client char))

(defgeneric state-value (client aspect))

;;; Octet Interface

(defgeneric stream-read-octet (stream))

(defgeneric stream-write-octet (stream octet))

(defgeneric stream-read-octets (stream octets &optional start end))

(defgeneric stream-write-octets (stream octets &optional start end))

(defgeneric read-element (transcoder stream))

(defgeneric write-element (transcoder stream element))

(defparameter *octet-transcoders* nil)

(defun check-stream (object)
  (unless (streamp object)
    (error 'type-error :datum object :expected-type '(satisfies streamp))))

(defun check-input-stream (object)
  (unless (input-stream-p object)
    (error 'type-error :datum object :expected-type '(satisfies input-stream-p))))

(defun check-output-stream (object)
  (unless (output-stream-p object)
    (error 'type-error :datum object :expected-type '(satisfies output-stream-p))))

(defun check-open-stream (object)
  (unless (open-stream-p object)
    (error 'stream-error :stream object)))

(defun check-character-stream (object)
  (unless (subtypep (stream-element-type object) 'character)
    (error 'stream-error :stream object)))

(defun check-binary-stream (object)
  (unless (subtypep (stream-element-type object) 'integer)
    (error 'stream-error :stream object)))

;;; Coerce

(defun coerce-input-stream (client designator)
  (cond ((null designator)
         (setf designator (state-value client 'cl:*standard-input*)))
        ((eq designator t)
         (setf designator (state-value client 'cl:*terminal-io*))))
  (check-input-stream designator)
  (check-open-stream designator)
  designator)

(defun coerce-output-stream (client designator)
  (cond ((null designator)
         (setf designator (state-value client 'cl:*standard-output*)))
        ((eq designator t)
         (setf designator (state-value client 'cl:*terminal-io*))))
  (check-output-stream designator)
  (check-open-stream designator)
  designator)

(defgeneric make-file-stream
    (client path direction if-exists if-does-not-exist element-type external-format))

;;; Interface definition

(defun ensure-symbol (name &optional (package *package*))
  (intern (string name) package))

(defmacro define-interface (client-var &key intrinsic)
  (let* ((intrinsic-pkg (if intrinsic (find-package '#:common-lisp) *package*))
         (open-sym (ensure-symbol '#:open intrinsic-pkg))
         (symbols '(broadcast-stream
                    broadcast-stream-streams
                    close
                    concatenated-stream
                    concatenated-stream-streams
                    echo-stream
                    echo-stream-input-stream
                    echo-stream-output-stream
                    file-stream
                    get-output-stream-string
                    input-stream-p
                    interactive-stream-p
                    make-broadcast-stream
                    make-concatenated-stream
                    make-echo-stream
                    make-string-input-stream
                    make-string-output-stream
                    make-synonym-stream
                    make-two-way-stream
                    open-stream-p
                    output-stream-p
                    pathname
                    stream
                    stream-element-type
                    stream-external-format
                    streamp
                    string-stream
                    synonym-stream
                    synonym-stream-symbol
                    truename
                    two-way-stream
                    two-way-stream-input-stream
                    two-way-stream-output-stream)))
    `(progn
       (shadowing-import ',symbols ,intrinsic-pkg)

       (export ',symbols ,intrinsic-pkg)

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
           ((var string &key index (start 0) end) &body body)
         (expand-with-input-from-string var string start end index body))

       (defmacro ,(ensure-symbol '#:with-output-to-string intrinsic-pkg)
           ((var &optional string-form &key (element-type ''character)) &body body)
         (expand-with-output-to-string var string-form element-type body))

       (defun ,(ensure-symbol '#:read-byte intrinsic-pkg)
           (stream &optional (eof-error-p t) eof-value)
         (check-input-stream stream)
         (let ((b (stream-read-byte stream)))
           (check-type b (or integer (eql :eof)))
           (if (eql b :eof)
               (if eof-error-p
                   (error 'end-of-file :stream stream)
                   eof-value)
               b)))

       (defun ,(ensure-symbol '#:write-byte intrinsic-pkg)
           (byte stream)
         (check-output-stream stream)
         (stream-write-byte stream byte))

       (defun ,(ensure-symbol '#:read-sequence intrinsic-pkg)
           (sequence stream &key (start 0) end)
         (check-type start unsigned-byte)
         (check-type end (or null unsigned-byte))
         (stream-read-sequence (coerce-input-stream ,client-var stream) sequence
                               start end))

       (defun ,(ensure-symbol '#:write-sequence intrinsic-pkg)
           (sequence stream &key (start 0) end)
         (check-type start unsigned-byte)
         (check-type end (or null unsigned-byte))
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
         (prog ((s (coerce-input-stream ,client-var stream))
               ch)
          repeat
            (setf ch (stream-peek-char s))
            (when (eq ch :eof)
              (when eof-error-p
                (error 'end-of-file :stream s))
              (return eof-value))
            (when (and peek-type
                       (or (and (eq peek-type t)
                                (whitespace-char-p ,client-var ch))
                           (and (not (eq peek-type t))
                                (char/= peek-type ch))))
              (stream-read-char s)
              (go repeat))
            (return ch)))

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
           (&optional control &rest arguments
            &aux (query-io (state-value ,client-var 'cl:*query-io*)))
         (declare (dynamic-extent arguments))
         (when control
           (stream-fresh-line query-io)
           (apply (state-value ,client-var 'format)
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
           (stream-write-string query-io "Please respond with \"y\" or \"n\". ")))

       (defun ,(ensure-symbol '#:yes-or-no-p intrinsic-pkg)
           (&optional control &rest arguments
            &aux (query-io (state-value ,client-var 'cl:*query-io*)))
         (declare (dynamic-extent arguments))
         (when control
           (stream-fresh-line query-io)
           (apply (state-value ,client-var 'format)
                  query-io control arguments)
           (stream-write-char query-io #\Space))
         (format *query-io* "(Yes or No) ")
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
           (stream-finish-output query-io)))

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

