(cl:in-package #:cyclosis)

(defclass string-stream (stream) ())

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

(defun expand-with-output-to-string
    (var string-form element-type body)
  (expand-with-open-stream var
                           (if string-form
                               `(make-string-output-stream :string ,string-form :element-type ,element-type)
                               `(make-string-output-stream :element-type ,element-type))
                           `(,@body
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

(defun expand-with-input-from-string
    (var string start end index body)
  (if index
      (multiple-value-bind (body-forms declares)
          (alexandria:parse-body body)
        (expand-with-open-stream var
                                 `(make-string-input-stream ,string ,start ,end)
                                 `(@declares
                                   (multiple-value-prog1
                                       (progn ,@body-forms)
                                     (setf ,index (string-input-stream-position ,var))))))
      (expand-with-open-stream var
                               `(make-string-input-stream ,string ,start ,end)
                               body)))
