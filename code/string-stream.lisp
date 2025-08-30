(cl:in-package #:cyclosis)

(defclass string-stream (stream)
  ((buffer :accessor buffer
           :initarg :buffer
           :type string)))

(defmethod initialize-instance :after ((instance string-stream) &rest initargs &key)
  (declare (ignore initargs))
  (check-type (buffer instance) string))

;;; String output stream and with-output-to-string.

(defclass string-output-stream
    (character-output-mixin string-stream
     fundamental-character-output-stream)
  ())

(defmethod initialize-instance :after ((instance string-output-stream) &rest initargs &key)
  (declare (ignore initargs))
  (unless (array-has-fill-pointer-p (buffer instance))
    (error "~S must be a string with a fill-pointer"
           (buffer instance))))

(defmethod stream-write-char ((stream string-output-stream) character)
  (vector-push-extend character (buffer stream)))

(defmethod stream-write-sequence
    ((stream string-output-stream) seq &optional (start 0) end)
  (with-accessors ((buffer buffer))
      stream
    (unless end
      (setf end (length seq)))
    (map nil (lambda (ch)
               (update-output-cursor stream ch))
         seq)
    (let ((current-length (length buffer))
          (new-length (+ (length buffer) (- end start))))
      (when (< (array-dimension buffer 0) new-length)
        (adjust-array buffer new-length))
      (setf (fill-pointer buffer) new-length)
      (replace buffer seq
               :start1 current-length
               :start2 start
               :end2 end)
      seq)))

(defmethod stream-file-position ((stream string-output-stream) &optional position)
  (if position
      nil
      (length (buffer stream))))

(defmethod stream-element-type ((stream string-output-stream))
  (array-element-type (buffer stream)))

(defun expand-with-output-to-string
    (var string-form element-type body)
  (if string-form
      (expand-with-open-stream var
                               `(progn ; element-type is ignored, but it might have side effects
                                  ,element-type
                                  (make-instance 'string-output-stream
                                                 :buffer (progn ,string-form)))
                               body)
      (expand-with-open-stream var
                               `(make-instance 'string-output-stream
                                               :buffer (make-array 8
                                                                   :element-type ,element-type
                                                                   :fill-pointer 0 :adjustable t))
                               `(,@body
                                 (copy-seq (buffer ,var))))))

;;; String input stream and with-input-from-string.

(defclass string-input-stream
    (character-input-mixin
     fundamental-character-input-stream
     string-stream)
  ((index :accessor index
          :initarg :index
          :initform nil)
   (start :accessor start
          :initarg :start
          :initform 0)
   (end :accessor end
        :initarg :end
        :initform nil)))

(defmethod initialize-instance :after ((instance string-input-stream) &rest initargs &key)
  (declare (ignore initargs))
  (with-accessors ((index index)
                   (start start)
                   (end end)
                   (buffer buffer))
      instance
    (unless index
      (setf index start))
    (unless end
      (setf end (length buffer)))))

(defmethod stream-read-char ((stream string-input-stream))
  (with-accessors ((index index)
                   (end end)
                   (buffer buffer))
      stream
    (if (< index end)
        (prog1
            (char buffer index)
          (incf index))
        :eof)))

(defmethod stream-peek-char ((stream string-input-stream))
  (with-accessors ((index index)
                   (end end)
                   (buffer buffer))
      stream
    (if (< index end)
        (char buffer index)
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
                                 `(make-instance 'string-input-stream
                                                 :buffer ,string
                                                 :start ,start
                                                 :end ,end)
                                 `(,@declares
                                   (multiple-value-prog1
                                       (progn ,@body-forms)
                                     (setf ,index (index ,var))))))
      (expand-with-open-stream var
                               `(make-instance 'string-input-stream
                                               :buffer ,string
                                               :start ,start
                                               :end ,end)
                               body)))
