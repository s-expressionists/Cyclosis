;;;; Implementations of the standard stream types.

(cl:in-package :cyclosis)

(defclass file-stream (stream) ())
(defclass string-stream (stream) ())

;;; Synonym stream.

(defclass synonym-stream (fundamental-stream)
  ((%symbol :initarg :symbol :reader synonym-stream-symbol)))

(defun make-synonym-stream (symbol)
  (check-type symbol symbol)
  (make-instance 'synonym-stream :symbol symbol))

(defmethod print-object ((object synonym-stream) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "for ~S" (synonym-stream-symbol object))))

(defun follow-synonym-stream (stream)
  (symbol-value (synonym-stream-symbol stream)))

(defmethod stream-element-type ((stream synonym-stream))
  (stream-element-type (follow-synonym-stream stream)))

(defmethod stream-external-format ((stream synonym-stream))
  (stream-external-format (follow-synonym-stream stream)))

(defmethod close ((stream synonym-stream) &key abort)
  (close (follow-synonym-stream stream) :abort abort))

(defmethod open-stream-p ((stream synonym-stream))
  (open-stream-p (follow-synonym-stream stream)))

(defmethod input-stream-p ((stream synonym-stream))
  (input-stream-p (follow-synonym-stream stream)))

(defmethod output-stream-p ((stream synonym-stream))
  (output-stream-p (follow-synonym-stream stream)))

(defmethod interactive-stream-p ((stream synonym-stream))
  (interactive-stream-p (follow-synonym-stream stream)))

(defmethod stream-file-position
    ((stream synonym-stream) &optional (position-spec nil position-spec-p))
  (if position-spec-p
      (file-position (follow-synonym-stream stream) position-spec)
      (file-position (follow-synonym-stream stream))))

(defmethod stream-file-length ((stream synonym-stream))
  (file-length (follow-synonym-stream stream)))

(defmethod stream-file-string-length ((stream synonym-stream) string)
  (file-string-length (follow-synonym-stream stream) string))

(defmethod stream-clear-input ((stream synonym-stream))
  (clear-input (follow-synonym-stream stream)))

(defmethod stream-read-sequence
    ((stream synonym-stream) seq &optional (start 0) end)
  (read-sequence seq (follow-synonym-stream stream) :start start :end end))

(defmethod stream-clear-output ((stream synonym-stream))
  (clear-output (follow-synonym-stream stream)))

(defmethod stream-finish-output ((stream synonym-stream))
  (finish-output (follow-synonym-stream stream)))

(defmethod stream-force-output ((stream synonym-stream))
  (force-output (follow-synonym-stream stream)))

(defmethod stream-write-sequence
    ((stream synonym-stream) seq &optional (start 0) end)
  (write-sequence seq (follow-synonym-stream stream) :start start :end end))

(defmethod stream-read-byte ((stream synonym-stream))
  (read-byte (follow-synonym-stream stream) nil :eof))

(defmethod stream-write-byte ((stream synonym-stream) integer)
  (write-byte integer (follow-synonym-stream stream)))

(defmethod stream-peek-char ((stream synonym-stream))
  (peek-char nil (follow-synonym-stream stream) nil :eof))

(defmethod stream-read-char-no-hang ((stream synonym-stream))
  (read-char-no-hang (follow-synonym-stream stream) nil :eof))

(defmethod stream-read-char ((stream synonym-stream))
  (read-char (follow-synonym-stream stream) nil :eof))

(defmethod stream-read-line ((stream synonym-stream))
  (read-line (follow-synonym-stream stream) nil ""))

(defmethod stream-listen ((stream synonym-stream))
  (listen (follow-synonym-stream stream)))

(defmethod stream-unread-char ((stream synonym-stream) character)
  (unread-char character (follow-synonym-stream stream)))

(defmethod stream-advance-to-column ((stream synonym-stream) column)
  (advance-to-column column (follow-synonym-stream stream)))

(defmethod stream-fresh-line ((stream synonym-stream))
  (fresh-line (follow-synonym-stream stream)))

(defmethod stream-line-column ((stream synonym-stream))
  (line-column (follow-synonym-stream stream)))

(defmethod stream-line-length ((stream synonym-stream))
  (line-length (follow-synonym-stream stream)))

(defmethod stream-start-line-p ((stream synonym-stream))
  (start-line-p (follow-synonym-stream stream)))

(defmethod stream-terpri ((stream synonym-stream))
  (terpri (follow-synonym-stream stream)))

(defmethod stream-write-char ((stream synonym-stream) character)
  (write-char character (follow-synonym-stream stream)))

(defmethod stream-write-string
    ((stream synonym-stream) string &optional (start 0) end)
  (write-string string (follow-synonym-stream stream) :start start :end end))

;;; Broadcast stream.

(defclass broadcast-stream (fundamental-output-stream)
  ((%streams :initarg :streams :reader broadcast-stream-streams)))

(defun make-broadcast-stream (&rest streams)
  (dolist (stream streams)
    (when (not (and (streamp stream)
                    (output-stream-p stream)))
      (error 'type-error
             :expected-type 'output-stream
             :datum stream)))
  (make-instance 'broadcast-stream :streams streams))

(defmacro broadcast-stream-op ((substream broadcast-stream default) &body body)
  `(loop
      with result = ,default
      for ,substream in (broadcast-stream-streams ,broadcast-stream)
      do (setf result (progn ,@body))
      finally (return result)))

(defmethod stream-write-char ((stream broadcast-stream) character)
  (broadcast-stream-op (substream stream character)
    (write-char character substream)))

(defmethod stream-write-byte ((stream broadcast-stream) byte)
  (broadcast-stream-op (substream stream byte)
    (write-byte byte substream)))

(defmethod stream-write-sequence
    ((stream broadcast-stream) seq &optional (start 0) end)
  (broadcast-stream-op (substream stream start)
    (write-sequence seq substream :start start :end end)))

(defmethod stream-element-type ((stream broadcast-stream))
  (broadcast-stream-op (substream stream t)
    (stream-element-type substream)))

(defmethod stream-fresh-line ((stream broadcast-stream))
  (broadcast-stream-op (substream stream nil)
    (fresh-line substream)))

(defmethod stream-file-length ((stream broadcast-stream))
  (broadcast-stream-op (substream stream 0)
    (file-length substream)))

(defmethod stream-file-string-length ((stream broadcast-stream) string)
  (broadcast-stream-op (substream stream 1)
    (file-string-length substream)))

(defmethod stream-file-position
    ((stream broadcast-stream) &optional (position-spec nil position-spec-p))
  (if position-spec-p
      (broadcast-stream-op (substream stream nil)
        (file-position substream position-spec))
      (broadcast-stream-op (substream stream 0)
        (file-position substream))))

(defmethod stream-line-column ((stream broadcast-stream))
  (broadcast-stream-op (substream stream nil)
    (line-column substream)))

(defmethod stream-line-length ((stream broadcast-stream))
  (broadcast-stream-op (substream stream nil)
    (line-length substream)))

(defmethod stream-advance-to-column ((stream broadcast-stream) column)
  (broadcast-stream-op (substream stream nil)
    (advance-to-column column substream)))

(defmethod stream-clear-output ((stream broadcast-stream))
  (broadcast-stream-op (substream stream nil)
    (clear-output substream)))

(defmethod stream-finish-output ((stream broadcast-stream))
  (broadcast-stream-op (substream stream nil)
    (finish-output substream)))

(defmethod stream-force-output ((stream broadcast-stream))
  (broadcast-stream-op (substream stream nil)
    (force-output substream)))

(defmethod stream-start-line-p ((stream broadcast-stream))
  (broadcast-stream-op (substream stream nil)
    (start-line-p substream)))

(defmethod stream-terpri ((stream broadcast-stream))
  (broadcast-stream-op (substream stream nil)
    (terpri substream)))

(defmethod stream-write-string
    ((stream broadcast-stream) string &optional (start 0) end)
  (broadcast-stream-op (substream stream string)
    (write-string string substream :start start :end end)))

(defmethod stream-external-format ((stream broadcast-stream))
  (broadcast-stream-op (substream stream :default)
    (stream-external-format substream)))

;;; Echo stream.

(defclass echo-stream (fundamental-output-stream
                       fundamental-input-stream
                       unread-char-mixin)
  ((input-stream :initarg :input-stream
                 :reader echo-stream-input-stream)
   (output-stream :initarg :output-stream
                  :reader echo-stream-output-stream)))

(defun make-echo-stream (input-stream output-stream)
  (when (not (and (streamp input-stream)
                  (input-stream-p input-stream)))
    (error 'type-error :datum input-stream :expected-type 'input-stream))
  (when (not (and (streamp output-stream)
                  (output-stream-p output-stream)))
    (error 'type-error :datum output-stream :expected-type 'output-stream))
  (make-instance 'echo-stream
                 :input-stream input-stream
                 :output-stream output-stream))

(defmethod stream-element-type ((stream echo-stream))
  (let ((in (stream-element-type (echo-stream-input-stream stream)))
        (out (stream-element-type (echo-stream-output-stream stream))))
    (if (or (equal in out)
            (and (subtypep in out)
                 (subtypep out in)))
        in
        `(and ,in ,out))))

(defmethod stream-write-byte ((stream echo-stream) character)
  (write-byte character (echo-stream-output-stream stream)))

(defmethod stream-write-char ((stream echo-stream) character)
  (write-char character (echo-stream-output-stream stream)))

(defmethod stream-write-sequence ((stream echo-stream) seq &optional (start 0) end)
  (write-sequence seq (echo-stream-output-stream stream) :start start :end end))

(defmethod stream-read-byte ((stream echo-stream))
  (let ((c (read-byte (echo-stream-input-stream stream) nil :eof)))
    (when (integerp c)
      (write-byte c (echo-stream-output-stream stream)))
    c))

(defmethod stream-read-char ((stream echo-stream))
  (let ((c (read-char (echo-stream-input-stream stream) nil :eof)))
    (when (characterp c)
      (write-char c (echo-stream-output-stream stream)))
    c))

(defmethod stream-read-char-no-hang ((stream echo-stream))
  (let ((c (read-char-no-hang (echo-stream-input-stream stream) nil :eof)))
    (when (characterp c)
      (write-char c (echo-stream-output-stream stream)))
    c))

(defmethod stream-read-sequence ((stream echo-stream) seq &optional (start 0) end)
  (let ((result (read-sequence seq (echo-stream-input-stream stream) :start start :end end)))
    (write-sequence seq (echo-stream-output-stream stream) :start start :end result)
    result))

(defmethod stream-listen ((stream echo-stream))
  (listen (echo-stream-input-stream stream)))

(defmethod stream-clear-input ((stream echo-stream))
  (clear-input (echo-stream-input-stream stream)))

(defmethod stream-clear-output ((stream echo-stream))
  (clear-output (echo-stream-output-stream stream)))

(defmethod stream-finish-output ((stream echo-stream))
  (finish-output (echo-stream-output-stream stream)))

(defmethod stream-force-output ((stream echo-stream))
  (force-output (echo-stream-output-stream stream)))

(defmethod stream-peek-char ((stream echo-stream))
  ;; Don't echo when peeking.
  (peek-char nil (echo-stream-input-stream stream) nil :eof))

(defmethod stream-read-line ((stream echo-stream))
  (multiple-value-bind (line missing-newline-p)
      (read-line (echo-stream-input-stream stream) nil "")
    (if missing-newline-p
        (write-string line (echo-stream-output-stream stream))
        (write-line line (echo-stream-output-stream stream)))
    (values line missing-newline-p)))

(defmethod stream-fresh-line ((stream echo-stream))
  (fresh-line (echo-stream-output-stream stream)))

(defmethod stream-line-column ((stream echo-stream))
  (line-column (echo-stream-output-stream stream)))

(defmethod stream-advance-to-column ((stream echo-stream) column)
  (advance-to-column column (echo-stream-output-stream stream)))

(defmethod stream-line-length ((stream echo-stream))
  (line-length (echo-stream-output-stream stream)))

(defmethod stream-start-line-p ((stream echo-stream))
  (start-line-p (echo-stream-output-stream stream)))

(defmethod stream-terpri ((stream echo-stream))
  (terpri (echo-stream-output-stream stream)))

(defmethod stream-write-string
    ((stream echo-stream) string &optional (start 0) end)
  (write-string string (echo-stream-output-stream stream) :start start :end end))

;;; Two-way stream.

(defclass two-way-stream (fundamental-output-stream
                          fundamental-input-stream
                          unread-char-mixin)
  ((input-stream :initarg :input-stream
                 :reader two-way-stream-input-stream)
   (output-stream :initarg :output-stream
                  :reader two-way-stream-output-stream)))

(defun make-two-way-stream (input-stream output-stream)
  (when (not (and (streamp input-stream)
                  (input-stream-p input-stream)))
    (error 'type-error :datum input-stream :expected-type 'input-stream))
  (when (not (and (streamp output-stream)
                  (output-stream-p output-stream)))
    (error 'type-error :datum output-stream :expected-type 'output-stream))
  (make-instance 'two-way-stream
                 :input-stream input-stream
                 :output-stream output-stream))

(defmethod stream-element-type ((stream two-way-stream))
  (let ((in (stream-element-type (two-way-stream-input-stream stream)))
        (out (stream-element-type (two-way-stream-output-stream stream))))
    (if (or (equal in out)
            (and (subtypep in out)
                 (subtypep out in)))
        in
        `(and ,in ,out))))

(defmethod stream-write-byte ((stream two-way-stream) byte)
  (write-byte byte (two-way-stream-output-stream stream)))

(defmethod stream-write-char ((stream two-way-stream) character)
  (write-char character (two-way-stream-output-stream stream)))

(defmethod stream-write-sequence ((stream two-way-stream) seq &optional (start 0) end)
  (write-sequence seq (two-way-stream-output-stream stream) :start start :end end))

(defmethod stream-read-byte ((stream two-way-stream))
  (read-byte (two-way-stream-input-stream stream) nil :eof))

(defmethod stream-read-char ((stream two-way-stream))
  (read-char (two-way-stream-input-stream stream) nil :eof))

(defmethod stream-read-char-no-hang ((stream two-way-stream))
  (read-char-no-hang (two-way-stream-input-stream stream) nil :eof))

(defmethod stream-read-sequence ((stream two-way-stream) seq &optional (start 0) end)
  (read-sequence seq (two-way-stream-input-stream stream) :start start :end end))

(defmethod stream-listen ((stream two-way-stream))
  (listen (two-way-stream-input-stream stream)))

(defmethod stream-clear-input ((stream two-way-stream))
  (clear-input (two-way-stream-input-stream stream)))

(defmethod stream-clear-output ((stream two-way-stream))
  (clear-output (two-way-stream-output-stream stream)))

(defmethod stream-finish-output ((stream two-way-stream))
  (finish-output (two-way-stream-output-stream stream)))

(defmethod stream-force-output ((stream two-way-stream))
  (force-output (two-way-stream-output-stream stream)))

(defmethod stream-peek-char ((stream two-way-stream))
  (peek-char nil (two-way-stream-input-stream stream) nil :eof))

(defmethod stream-read-line ((stream two-way-stream))
  (read-line (two-way-stream-input-stream stream) nil ""))

(defmethod stream-fresh-line ((stream two-way-stream))
  (fresh-line (two-way-stream-output-stream stream)))

(defmethod stream-line-column ((stream two-way-stream))
  (line-column (two-way-stream-output-stream stream)))

(defmethod stream-advance-to-column ((stream two-way-stream) column)
  (advance-to-column column (two-way-stream-output-stream stream)))

(defmethod stream-line-length ((stream two-way-stream))
  (line-length (two-way-stream-output-stream stream)))

(defmethod stream-start-line-p ((stream two-way-stream))
  (start-line-p (two-way-stream-output-stream stream)))

(defmethod stream-terpri ((stream two-way-stream))
  (terpri (two-way-stream-output-stream stream)))

(defmethod stream-write-string
    ((stream two-way-stream) string &optional (start 0) end)
  (write-string string (two-way-stream-output-stream stream) :start start :end end))

;;; Concatenated stream.

(defclass concatenated-stream (fundamental-character-input-stream ; For the default stream-read-line method.
                               fundamental-input-stream
                               unread-char-mixin)
  ((streams :initarg :streams :reader concatenated-stream-streams)))

(defun make-concatenated-stream (&rest input-streams)
  (dolist (s input-streams)
    (when (not (and (streamp s)
                    (input-stream-p s)))
      (error 'type-error :datum s :expected-type 'input-stream)))
  (make-instance 'concatenated-stream :streams input-streams))

(defmethod stream-element-type ((stream concatenated-stream))
  (if (concatenated-stream-streams stream)
      (stream-element-type (first (concatenated-stream-streams stream)))
      nil))

(defmethod stream-read-sequence
    ((stream concatenated-stream) seq &optional (start 0) end)
  (setf end (or end (length seq)))
  (loop
     (when (>= start end)
       (return end))
     (when (endp (concatenated-stream-streams stream))
       (return start))
     (let ((next (read-sequence seq (first (concatenated-stream-streams stream))
                                :start start
                                :end end)))
       (when (eql next start)
         ;; Reached end of this stream. Pop streams.
         (pop (slot-value stream 'streams)))
       (setf start next))))

(defmethod stream-read-byte ((stream concatenated-stream))
  (loop
     (when (endp (concatenated-stream-streams stream))
       (return :eof))
     (let ((ch (read-byte (first (concatenated-stream-streams stream)) nil :eof)))
       (when (not (eql ch :eof))
         (return ch))
       ;; Reached end of this stream. Pop streams.
       (pop (slot-value stream 'streams)))))

(defmethod stream-read-char ((stream concatenated-stream))
  (loop
     (when (endp (concatenated-stream-streams stream))
       (return :eof))
     (let ((ch (read-char (first (concatenated-stream-streams stream)) nil :eof)))
       (when (not (eql ch :eof))
         (return ch))
       ;; Reached end of this stream. Pop streams.
       (pop (slot-value stream 'streams)))))

(defmethod stream-read-char-no-hang ((stream concatenated-stream))
  (loop
     (when (endp (concatenated-stream-streams stream))
       (return :eof))
     (let ((ch (read-char-no-hang (first (concatenated-stream-streams stream)) nil :eof)))
       (when (not (eql ch :eof))
         (return ch))
       ;; Reached end of this stream. Pop streams.
       (pop (slot-value stream 'streams)))))

(defmethod stream-listen ((stream concatenated-stream))
  ;; Built on top of READ-CHAR-NO-HANG because LISTEN cannot distinguish
  ;; between blocking & EOF.
  (loop
     (when (endp (concatenated-stream-streams stream))
       (return nil))
     (let ((ch (read-char-no-hang (first (concatenated-stream-streams stream)) nil :eof)))
       (case ch
         (:eof
          ;; Reached end of this stream. Pop streams.
          (pop (slot-value stream 'streams)))
         (nil
          (return nil))
         (t
          (unread-char ch (first (concatenated-stream-streams stream)))
          (return t))))))

(defmethod stream-clear-input ((stream concatenated-stream))
  (if (concatenated-stream-streams stream)
      (clear-input (first (concatenated-stream-streams stream)))
      nil))

(defmethod stream-peek-char ((stream concatenated-stream))
  (loop
     (when (endp (concatenated-stream-streams stream))
       (return :eof))
     (let ((ch (peek-char nil (first (concatenated-stream-streams stream)) nil :eof)))
       (when (not (eql ch :eof))
         (return ch))
       ;; Reached end of this stream. Pop streams.
       (pop (slot-value stream 'streams)))))

;;; String output stream and with-output-to-string.

(defclass string-output-stream (fundamental-character-output-stream
                                string-stream)
  ((element-type :initarg :element-type :reader string-output-stream-element-type)
   (string :initarg :string :accessor string-output-stream-string))
  (:default-initargs :string nil))

(defun make-string-output-stream
    (&key (element-type 'character) (string nil stringp))
  (when stringp
    (when (not (and (stringp string)
                    (array-has-fill-pointer-p string)))
      (error "~S must be a string with a fill-pointer" string)))
  (when (not (subtypep element-type 'character))
    (error "Element-type ~S must be a subtype of CHARACTER" element-type))
  (make-instance 'string-output-stream :element-type element-type :string string))

(defun get-output-stream-string (string-output-stream)
  (check-type string-output-stream string-output-stream)
  (prog1 (or (string-output-stream-string string-output-stream)
             (make-array 0 :element-type (string-output-stream-element-type string-output-stream)))
    (setf (string-output-stream-string string-output-stream) nil)))

(defmethod stream-write-char ((stream string-output-stream) character)
  (unless (string-output-stream-string stream)
    (setf (string-output-stream-string stream)
          (make-array 8
                      :element-type (string-output-stream-element-type stream)
                      :adjustable t
                      :fill-pointer 0)))
  (vector-push-extend character (string-output-stream-string stream)))

(defmethod stream-write-sequence
    ((stream string-output-stream) seq &optional (start 0) end)
  (setf end (or end (length seq)))
  (when (not (typep seq 'string))
    ;; Make sure the sequence only contains characters.
    (loop
       for i from start below end
       for elt = (elt seq i)
       when (not (characterp elt))
       do (error 'simple-type-error
                 :expected-type 'character
                 :datum elt
                 :format-control "Non-character in sequence ~S"
                 :format-arguments (list seq))))
  (let ((n-chars (- end start)))
    (unless (string-output-stream-string stream)
      (setf (string-output-stream-string stream)
            (make-array (max n-chars 8)
                        :element-type (string-output-stream-element-type stream)
                        :adjustable t
                        :fill-pointer 0)))
    (let* ((output (string-output-stream-string stream))
           (current-length (length output))
           (new-length (+ (length output) n-chars)))
      (when (< (array-dimension output 0) new-length)
        (adjust-array output new-length))
      (setf (fill-pointer output) new-length)
      (replace output seq
               :start1 current-length
               :start2 start
               :end2 end)
      seq)))

(defmethod stream-start-line-p ((stream string-output-stream))
  ;; If the string is empty or last character is a newline, then it's
  ;; at the start.
  (let ((string (string-output-stream-string stream)))
    (or (zerop (length string))
        (eql (char string (1- (length string))) #\Newline))))

(defmethod stream-line-column ((stream string-output-stream))
  (let ((string (string-output-stream-string stream)))
    (cond (string
           (let ((column 0))
             (loop
                (when (or (eql (length string) column)
                          (eql (char string (- (length string) column 1)) #\Newline))
                  (return column))
                (incf column))))
          (t
           0))))

(defmacro with-output-to-string
    ((var &optional string-form &key (element-type ''character)) &body body)
  (if string-form
      `(with-open-stream (,var (make-string-output-stream :string ,string-form :element-type ,element-type))
         ,@body)
      `(with-open-stream (,var (make-string-output-stream :element-type ,element-type))
         ,@body
         (get-output-stream-string ,var))))

;;; String input stream and with-input-from-string.

(defclass string-input-stream (fundamental-character-input-stream
                               unread-char-mixin
                               string-stream)
  ((string :initarg :string)
   (start :initarg :start :reader string-input-stream-position)
   (end :initarg :end)))

(defun make-string-input-stream (string &optional (start 0) end)
  (check-type string string)
  (make-instance 'string-input-stream
                 :string string
                 :start start
                 :end (or end (length string))))

(defmethod stream-read-char ((stream string-input-stream))
  (if (< (slot-value stream 'start) (slot-value stream 'end))
      (prog1 (char (slot-value stream 'string)
                   (slot-value stream 'start))
        (incf (slot-value stream 'start)))
      :eof))

(defmethod stream-read-sequence
    ((stream string-input-stream) seq &optional (start 0) end)
  (let* ((available (- (slot-value stream 'end) (slot-value stream 'start)))
         (requested (- (or end (length seq)) start))
         (provided (min available requested)))
    (replace seq (slot-value stream 'string)
             :start1 start
             :end1 (+ start provided)
             :start2 (slot-value stream 'start))
    (incf (slot-value stream 'start) provided)
    (+ start provided)))

(defmacro with-input-from-string
    ((var string &key (start 0) end index) &body body)
  (cond (index
         (multiple-value-bind (body-forms declares)
             (alexandria:parse-body body)
           `(with-open-stream (,var (make-string-input-stream ,string ,start ,end))
              ,@declares
              (multiple-value-prog1
                  (progn ,@body-forms)
                (setf ,index (string-input-stream-position ,var))))))
        (t
         `(with-open-stream (,var (make-string-input-stream ,string ,start ,end))
            ,@body))))
