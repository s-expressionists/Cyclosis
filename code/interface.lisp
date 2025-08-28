(cl:in-package #:cyclosis)

;;; The standard stream class.

(defclass stream () ())

(defclass file-stream (stream) ())

;;; Gray Streams classes.

(defclass fundamental-stream (stream)
  ((%openp :accessor stream-open-p
           :initform t
           :type boolean))
  (:documentation "The base class for all Gray streams."))

(defclass fundamental-input-stream (fundamental-stream)
  ()
  (:documentation "A superclass of all Gray input streams."))

(defclass fundamental-output-stream (fundamental-stream)
  ()
  (:documentation "A superclass of all Gray output streams."))

(defclass fundamental-character-stream (fundamental-stream)
  ()
  (:documentation "A superclass of all Gray streams whose element-type is a subtype of
 character."))

(defclass fundamental-binary-stream (fundamental-stream)
  ()
  (:documentation "A superclass of all Gray streams whose element-type is a subtype of
 unsigned-byte or signed-byte."))

(defclass fundamental-character-input-stream
    (fundamental-input-stream fundamental-character-stream)
  ()
  (:documentation "A superclass of all Gray input streams whose element-type is a
subtype of unsigned-byte or signed-byte."))

(defclass fundamental-character-output-stream
    (fundamental-output-stream fundamental-character-stream)
  ()
  (:documentation "A superclass of all Gray output streams whose element-type is a
subtype of character."))

(defclass fundamental-binary-input-stream
    (fundamental-input-stream fundamental-binary-stream)
  ()
  (:documentation "A superclass of all Gray input streams whose element-type is a
subtype of unsigned-byte or signed-byte."))

(defclass fundamental-binary-output-stream
    (fundamental-output-stream fundamental-binary-stream)
  ()
  (:documentation "A superclass of all Gray output streams whose element-type is a
subtype of unsigned-byte or signed-byte."))

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

;;; Octet Interface

(defgeneric stream-read-octet (stream))

(defgeneric stream-write-octet (stream octet))

(defgeneric stream-read-octets (stream octets &optional start end))

(defgeneric stream-write-octets (stream octets &optional start end))

(defgeneric read-element (transcoder stream))

(defgeneric write-element (transcoder stream element))

(defgeneric encoded-length (transcoder elements))

(defgeneric element-length (transcoder)
  (:method (transcoder)
    (declare (ignore))
    1))

(defparameter *default-character-external-format* :utf-8)

(defparameter *default-binary-external-format* :be)

(defgeneric make-transcoder (external-format element-type &rest options)
  (:method (external-format element-type &rest options)
    (declare (ignore options))
    (error "Unable to find transcoder for element-type ~s and external-format ~s"
           element-type external-format)))

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
  ;; This bizarre logic is needed because CL:FILE-STREAM-LENGTH is
  ;; required to return 1 for an empty broadcast stream while
  ;; CL:STREAM-ELEMENT-TYPE is required to return T. These two
  ;; requirements are contradictions, but the former normally requires
  ;; a character stream, but the latter means that it is not a
  ;; character stream.
  (unless (or (subtypep (stream-element-type object) 'character)
              (and (typep object 'broadcast-stream)
                   (null (broadcast-stream-streams object))))
    (error 'stream-error :stream object)))

(defun check-binary-stream (object)
  (unless (subtypep (stream-element-type object) 'integer)
    (error 'stream-error :stream object)))

;;; Coerce

(defun coerce-input-stream (client designator)
  (cond ((null designator)
         (setf designator (trinsic:cell-value client 'cl:*standard-input* 'cl:variable)))
        ((eq designator t)
         (setf designator (trinsic:cell-value client 'cl:*terminal-io* 'cl:variable))))
  (check-input-stream designator)
  (check-open-stream designator)
  designator)

(defun coerce-output-stream (client designator)
  (cond ((null designator)
         (setf designator (trinsic:cell-value client 'cl:*standard-output* 'cl:variable)))
        ((eq designator t)
         (setf designator (trinsic:cell-value client 'cl:*terminal-io* 'cl:variable))))
  (check-output-stream designator)
  (check-open-stream designator)
  designator)

(defgeneric make-file-stream
    (client path direction if-exists if-does-not-exist element-type external-format))

;;; Interface definition

(defun ensure-symbol (name &optional (package *package*))
  (intern (string name) package))

(trinsic:make-define-interface
    (:client-form client-var :client-class client-class :intrinsic intrinsicp)
    ((standard-input-sym cl:*standard-input* :variable t)
     (standard-output-sym cl:*standard-output* :variable t)
     (error-output-sym cl:*error-output* :variable t)
     (trace-output-sym cl:*trace-output* :variable t)
     (terminal-io-sym cl:*terminal-io* :variable t)
     (debug-io-sym cl:*debug-io* :variable t)
     (query-io-sym cl:*query-io* :variable t)
     (advance-to-column-sym #:advance-to-column)
     (broadcast-stream-streams-sym cl:broadcast-stream-streams)
     (broadcast-stream-sym cl:broadcast-stream)
     (clear-input-sym cl:clear-input)
     (clear-output-sym cl:clear-output)
     (close-sym cl:close)
     (concatenated-stream-streams-sym cl:concatenated-stream-streams)
     (concatenated-stream-sym cl:concatenated-stream)
     (echo-stream-input-stream-sym cl:echo-stream-input-stream)
     (echo-stream-output-stream-sym cl:echo-stream-output-stream)
     (echo-stream-sym cl:echo-stream)
     (file-length-sym cl:file-length)
     (file-position-sym cl:file-position)
     (file-stream-sym cl:file-stream)
     (file-string-length-sym cl:file-string-length)
     (finish-output-sym cl:finish-output)
     (force-output-sym cl:force-output)
     (fresh-line-sym cl:fresh-line)
     (get-output-stream-string-sym cl:get-output-stream-string)
     (input-stream-p-sym cl:input-stream-p)
     (interactive-stream-p-sym cl:interactive-stream-p)
     (line-column-sym #:line-column)
     (line-length-sym #:line-length)
     (listen-sym cl:listen)
     (make-broadcast-stream-sym cl:make-broadcast-stream)
     (make-concatenated-stream-sym cl:make-concatenated-stream)
     (make-echo-stream-sym cl:make-echo-stream)
     (make-string-input-stream-sym cl:make-string-input-stream)
     (make-string-output-stream-sym cl:make-string-output-stream)
     (make-synonym-stream-sym cl:make-synonym-stream)
     (make-two-way-stream-sym cl:make-two-way-stream)
     (open-stream-p-sym cl:open-stream-p)
     (open-sym cl:open)
     (output-stream-p-sym cl:output-stream-p)
     (pathname-sym cl:pathname)
     (peek-char-sym cl:peek-char)
     (read-byte-sym cl:read-byte)
     (read-char-no-hang-sym cl:read-char-no-hang)
     (read-char-sym cl:read-char)
     (read-line-sym cl:read-line)
     (read-sequence-sym cl:read-sequence)
     (start-line-p-sym #:start-line-p)
     (stream-element-type-sym cl:stream-element-type)
     (stream-external-format-sym cl:stream-external-format)
     (stream-sym cl:stream)
     (streamp-sym cl:streamp)
     (string-stream-sym cl:string-stream)
     (synonym-stream-sym cl:synonym-stream)
     (synonym-stream-symbol-sym cl:synonym-stream-symbol)
     (terpri-sym cl:terpri)
     (truename-sym cl:truename)
     (two-way-stream-input-stream-sym cl:two-way-stream-input-stream)
     (two-way-stream-output-stream-sym cl:two-way-stream-output-stream)
     (two-way-stream-sym cl:two-way-stream)
     (unread-char-sym cl:unread-char)
     (with-input-from-string-sym cl:with-input-from-string)
     (with-open-file-sym cl:with-open-file)
     (with-open-stream-sym cl:with-open-stream)
     (with-output-to-string-sym cl:with-output-to-string)
     (write-byte-sym cl:write-byte)
     (write-char-sym cl:write-char)
     (write-line-sym cl:write-line)
     (write-sequence-sym cl:write-sequence)
     (write-string-sym cl:write-string)
     (y-or-n-p-sym cl:y-or-n-p)
     (yes-or-no-p-sym cl:yes-or-no-p))
  (let* ((intrinsic-pkg (if intrinsicp (find-package '#:common-lisp) *package*))
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
    `((locally
          (declare (special ,standard-input-sym
                            ,standard-output-sym
                            ,error-output-sym
                            ,trace-output-sym
                            ,terminal-io-sym
                            ,debug-io-sym
                            ,query-io-sym)
                   #+sbcl (sb-ext:muffle-conditions sb-c::&optional-and-&key-in-lambda-list))
        (shadowing-import ',symbols ,intrinsic-pkg)

      (export ',symbols ,intrinsic-pkg)

      (defparameter ,standard-input-sym
        (trinsic:initial-cell-value ,client-var 'cl:*standard-input* 'cl:variable))

      (defparameter ,standard-output-sym
        (trinsic:initial-cell-value ,client-var 'cl:*standard-output* 'cl:variable))

      (defparameter ,error-output-sym
        (trinsic:initial-cell-value ,client-var 'cl:*error-output* 'cl:variable))

      (defparameter ,trace-output-sym
        (trinsic:initial-cell-value ,client-var 'cl:*trace-output* 'cl:variable))

      (defparameter ,terminal-io-sym
        (trinsic:initial-cell-value ,client-var 'cl:*terminal-io* 'cl:variable))

      (defparameter ,debug-io-sym
        (trinsic:initial-cell-value ,client-var 'cl:*debug-io* 'cl:variable))

      (defparameter ,query-io-sym
        (trinsic:initial-cell-value ,client-var 'cl:*query-io* 'cl:variable))

      (defun ,make-broadcast-stream-sym (&rest output-streams)
        "Returns a broadcast stream that has the indicated output-streams initially associated
with it."
        (make-instance 'broadcast-stream :streams output-streams))

      (defun ,make-concatenated-stream-sym (&rest input-streams)
        "Returns a concatenated stream that has the indicated input-streams initially
associated with it."
        (make-instance 'concatenated-stream :streams input-streams))

      (defun ,make-echo-stream-sym (input-stream output-stream)
        "Creates and returns an echo stream that takes input from input-stream and sends output
to output-stream."
        (make-instance 'echo-stream
                       :input-stream input-stream
                       :output-stream output-stream))

      (defun ,make-string-input-stream-sym (string &optional (start 0) end)
        "Returns an input string stream. This stream will supply, in order, the characters in
the substring of string bounded by start and end. After the last character has been supplied,
the string stream will then be at end of file."
        (make-instance 'string-input-stream
                       :string string
                       :start start
                       :position start
                       :end (or end (length string))))

      (defun ,make-string-output-stream-sym (&key (element-type 'character))
        "Returns an output string stream that accepts characters and makes available (via
get-output-stream-string) a string that contains the characters that were actually output."
        (make-instance 'string-output-stream
                       :string (make-array 8 :element-type element-type
                                             :fill-pointer 0 :adjustable t)))

      (defun ,make-synonym-stream-sym (symbol)
        "Returns a synonym stream whose synonym stream symbol is symbol."
        (make-instance 'synonym-stream :symbol symbol))

      (defun ,make-two-way-stream-sym (input-stream output-stream)
        "Returns a two-way stream that gets its input from input-stream and sends its output to
output-stream."
        (make-instance 'two-way-stream
                       :input-stream input-stream
                       :output-stream output-stream))

      (defun ,open-sym
          (filespec
           &key (direction :input) (element-type 'character) (if-exists nil if-exists-p)
                (if-does-not-exist nil if-does-not-exist-p) (external-format :default))
        (open ,client-var filespec direction element-type if-exists if-exists-p
              if-does-not-exist if-does-not-exist-p external-format))

      (defmacro ,with-open-stream-sym ((var stream) &body body)
        (expand-with-open-stream var stream body))

      (defmacro ,with-open-file-sym ((var filespec &rest options) &body body)
        (expand-with-open-file ',open-sym var filespec options body))

      (defmacro ,with-input-from-string-sym ((var string &key index (start 0) end) &body body)
        (expand-with-input-from-string var string start end index body))

      (defmacro ,with-output-to-string-sym
          ((var &optional string-form &key (element-type ''character)) &body body)
        (expand-with-output-to-string var string-form element-type body))

      (defun ,read-byte-sym (stream &optional (eof-error-p t) eof-value)
        (check-input-stream stream)
        (let ((b (stream-read-byte stream)))
          (check-type b (or integer (eql :eof)))
          (if (eql b :eof)
              (if eof-error-p
                  (error 'end-of-file :stream stream)
                  eof-value)
              b)))

      (defun ,write-byte-sym (byte stream)
        (check-output-stream stream)
        (stream-write-byte stream byte))

      (defun ,read-sequence-sym (sequence stream &key (start 0) end)
        (check-type start unsigned-byte)
        (check-type end (or null unsigned-byte))
        (stream-read-sequence (coerce-input-stream ,client-var stream) sequence
                              start end))

      (defun ,write-sequence-sym (sequence stream &key (start 0) end)
        (check-type start unsigned-byte)
        (check-type end (or null unsigned-byte))
        (stream-write-sequence (coerce-output-stream ,client-var stream) sequence
                               start end)
        sequence)

      (defun ,file-position-sym (stream &optional (position-spec nil position-spec-p))
        (check-stream stream)
        (cond (position-spec-p
               (check-type position-spec (or (integer 0) (member :start :end)))
               (stream-file-position stream position-spec))
              (t
               (stream-file-position stream))))

      (defun ,file-length-sym (stream)
        (check-stream stream)
        (stream-file-length stream))

      (defun ,file-string-length-sym (stream object)
        (check-output-stream stream)
        (check-character-stream stream)
        (check-type object (or string character))
        (when (characterp object)
          (setf object (string object)))
        (stream-file-string-length stream object))

      (defun ,read-char-sym (&optional stream (eof-error-p t) eof-value recursive-p)
        (declare (ignore recursive-p))
        (let* ((s (coerce-input-stream ,client-var stream))
               (c (stream-read-char s)))
          (check-type c (or character (eql :eof)))
          (cond ((eql c :eof)
                 (when eof-error-p
                   (error 'end-of-file :stream s))
                 eof-value)
                (c))))

      (defun ,read-char-no-hang-sym (&optional stream (eof-error-p t) eof-value recursive-p)
        (declare (ignore recursive-p))
        (let* ((s (coerce-input-stream ,client-var stream))
               (c (stream-read-char-no-hang s)))
          (check-type c (or character (eql :eof) null))
          (cond ((eql c :eof)
                 (when eof-error-p
                   (error 'end-of-file :stream s))
                 eof-value)
                (c))))

      (defun ,read-line-sym (&optional input-stream (eof-error-p t) eof-value recursive-p)
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

      (defun ,unread-char-sym (character &optional stream)
        (let ((s (coerce-input-stream ,client-var stream)))
          (check-type character character)
          (stream-unread-char s character)
          nil))

      (defun ,peek-char-sym (&optional peek-type stream (eof-error-p t) eof-value recursive-p)
        (peek-char ,client-var peek-type stream eof-error-p eof-value recursive-p))

      (defun ,clear-input-sym (&optional stream)
        (stream-clear-input (coerce-input-stream ,client-var stream))
        nil)

      (defun ,finish-output-sym (&optional output-stream)
        (stream-finish-output (coerce-output-stream ,client-var output-stream))
        nil)

      (defun ,force-output-sym (&optional output-stream)
        (stream-force-output (coerce-output-stream ,client-var output-stream))
        nil)

      (defun ,clear-output-sym (&optional output-stream)
        (stream-clear-output (coerce-output-stream ,client-var output-stream))
        nil)

      (defun ,write-char-sym (character &optional stream)
        (let ((s (coerce-output-stream ,client-var stream)))
          (check-type character character)
          (stream-write-char s character)
          character))

      (defun ,listen-sym (&optional input-stream)
        (stream-listen (coerce-input-stream ,client-var input-stream)))

      (defun ,y-or-n-p-sym (&optional control &rest arguments)
        (y-or-n-p ,client-var control arguments))

      (define-compiler-macro ,y-or-n-p-sym (&whole whole &optional control &rest arguments)
        (y-or-n-p/compiler-macro ,client-var whole control arguments))

      (defun ,yes-or-no-p-sym (&optional control &rest arguments)
        (yes-or-no-p ,client-var control arguments))

      (define-compiler-macro ,yes-or-no-p-sym (&whole whole &optional control &rest arguments)
        (yes-or-no-p/compiler-macro ,client-var whole control arguments))

      (defun ,write-string-sym (string &optional stream &key (start 0) end)
        (check-type string string)
        (stream-write-string (coerce-output-stream ,client-var stream) string start end)
        string)

      (defun ,write-line-sym (string &optional stream &key (start 0) end)
        (check-type string string)
        (let ((stream (coerce-output-stream ,client-var stream)))
          (stream-write-string stream string start end)
          (stream-terpri stream))
        string)

      (defun ,terpri-sym (&optional stream)
        (stream-terpri (coerce-output-stream ,client-var stream))
        nil)

      (defun ,fresh-line-sym (&optional stream)
        (stream-fresh-line (coerce-output-stream ,client-var stream)))

      (defun ,start-line-p-sym (&optional stream)
        (stream-start-line-p (coerce-output-stream ,client-var stream)))

      (defun ,line-column-sym (&optional stream)
        (stream-line-column (coerce-output-stream ,client-var stream)))

      (defun ,line-length-sym (&optional stream)
        (stream-line-length (coerce-output-stream ,client-var stream)))

      (defun ,advance-to-column-sym (column &optional stream)
        (stream-advance-to-column (coerce-output-stream ,client-var stream) column))))))
