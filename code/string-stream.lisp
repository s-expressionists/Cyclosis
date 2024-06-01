(cl:in-package #:cyclosis)

(defclass string-stream (stream)
  ((string :initarg :string
           :reader string-stream-string)))

;;; String output stream and with-output-to-string.

(defclass string-output-stream
    (character-output-mixin string-stream
     fundamental-character-output-stream)
  ())

(defmethod initialize-instance :after ((instance string-output-stream) &rest initargs)
  (declare (ignore initargs))
  (unless (array-has-fill-pointer-p (string-stream-string instance))
    (error "~S must be a string with a fill-pointer"
           (string-stream-string instance))))

(defun make-string-output-stream (&key (element-type 'character))
  (when (not (subtypep element-type 'character))
    (error "Element-type ~S must be a subtype of CHARACTER" element-type))
  (make-instance 'string-output-stream
                 :string (make-array 8 :element-type element-type
                                       :fill-pointer 0 :adjustable t)))

(defun get-output-stream-string (string-output-stream)
  (check-type string-output-stream string-output-stream)
  (with-accessors ((string string-stream-string))
      string-output-stream
    (prog1
        (copy-seq string)
      (reset-cursor (output-cursor string-output-stream))
      (setf (fill-pointer string) 0))))

(defmethod stream-write-char ((stream string-output-stream) character)
  (vector-push-extend character (string-stream-string stream)))

(defmethod stream-write-sequence
    ((stream string-output-stream) seq &optional (start 0) end)
  (with-accessors ((string string-stream-string))
      stream
    (setf end (or end (length seq)))
    (when (not (typep seq 'string))
      ;; Make sure the sequence only contains characters.
      (loop for i from start below end
            for elt = (elt seq i)
            when (not (characterp elt))
              do (error 'simple-type-error
                        :expected-type 'character
                        :datum elt
                        :format-control "Non-character in sequence ~S"
                        :format-arguments (list seq))))
    (map nil (lambda (ch)
               (update-output-cursor stream ch))
         seq)
    (let ((current-length (length string))
          (new-length (+ (length string) (- end start))))
      (when (< (array-dimension string 0) new-length)
        (adjust-array string new-length))
      (setf (fill-pointer string) new-length)
      (replace string seq
               :start1 current-length
               :start2 start
               :end2 end)
      seq)))

(defmethod stream-file-position ((stream string-output-stream) &optional position)
  (if position
      nil
      (length (string-stream-string stream))))

(defmethod stream-element-type ((stream string-output-stream))
  (array-element-type (string-stream-string stream)))

(defun expand-with-output-to-string
    (var string-form element-type body)
  (if string-form
      (expand-with-open-stream var
                               `(progn ; element-type is ignored, but it might have side effects
                                  ,element-type
                                  (make-instance 'string-output-stream
                                                 :string (progn ,string-form)))
                               body)
      (expand-with-open-stream var
                               `(make-string-output-stream :element-type ,element-type)
                               `(,@body
                                 (get-output-stream-string ,var)))))

;;; String input stream and with-input-from-string.

(defclass string-input-stream (character-input-mixin
                               fundamental-character-input-stream
                               string-stream)
  ((position :initarg :position
             :accessor string-input-stream-position)
   (start :initarg :start
          :reader string-input-stream-start)
   (end :initarg :end
        :reader string-input-stream-end)))

(defun make-string-input-stream (string &optional (start 0) end)
  (check-type string string)
  (make-instance 'string-input-stream
                 :string string
                 :start start
                 :position start
                 :end (or end (length string))))

(defmethod stream-read-char ((stream string-input-stream))
  (with-accessors ((position string-input-stream-position)
                   (end string-input-stream-end)
                   (string string-stream-string))
      stream
    (if (< position end)
        (prog1
            (char string position)
          (incf position))
        :eof)))

(defmethod stream-peek-char ((stream string-input-stream))
  (with-accessors ((position string-input-stream-position)
                   (end string-input-stream-end)
                   (string string-stream-string))
      stream
    (if (< position end)
        (char string position)
        :eof)))

#+(or)(defmethod stream-read-sequence
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
                                 `(,@declares
                                   (multiple-value-prog1
                                       (progn ,@body-forms)
                                     (setf ,index (string-input-stream-position ,var))))))
      (expand-with-open-stream var
                               `(make-string-input-stream ,string ,start ,end)
                               body)))
