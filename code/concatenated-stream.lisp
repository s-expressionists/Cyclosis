(cl:in-package #:cyclosis)

;;; Concatenated stream.

(defclass concatenated-stream (character-input-mixin fundamental-character-input-stream)
  ((%input-streams :accessor stream-input-streams
                   :initarg :input-streams
                   :type list))
  (:documentation "A concatenated stream is an input stream which is a composite stream of zero
or more other input streams, such that the sequence of data which can be read from the
concatenated stream is the same as the concatenation of the sequences of data which could be
read from each of the constituent streams."))

(defmethod initialize-instance :after ((instance concatenated-stream) &rest initargs &key)
  (declare (ignore initargs))
  (mapc #'check-input-stream (stream-input-streams instance)))

(defmacro concatenated-stream-read-element (stream method)
  `(with-accessors ((streams stream-input-streams))
       ,stream
     (prog ((el nil))
      next-stream
        (cond ((endp streams)
               (return :eof))
              ((eq :eof (setf el (,method (first streams))))
               ;; Reached end of this stream. Pop streams.
               (pop streams)
               (go next-stream))
              (t
               (return el))))))

(defmacro concatenated-stream-call-method (stream method fallback &rest args)
  (let ((streams-var (gensym "STREAMS")))
    `(let ((,streams-var (stream-input-streams ,stream)))
       (if ,streams-var
           (,method (car ,streams-var) ,@args)
           ,fallback))))

(defmacro concatenated-stream-setf (stream method new-value &rest args)
  (let ((streams-var (gensym "STREAMS")))
    `(let ((,streams-var (stream-input-streams ,stream)))
       (if ,streams-var
           (setf (,method (car ,streams-var) ,@args) ,new-value)
           ,new-value))))

(defmethod stream-read-byte ((stream concatenated-stream))
  (concatenated-stream-read-element stream stream-read-byte))

(defmethod stream-read-char ((stream concatenated-stream))
  (concatenated-stream-read-element stream stream-read-char))

(defmethod stream-read-char-no-hang ((stream concatenated-stream))
  (concatenated-stream-read-element stream stream-read-char-no-hang))

(defmethod stream-peek-char ((stream concatenated-stream))
  (concatenated-stream-read-element stream stream-peek-char))

(defmethod stream-listen ((stream concatenated-stream))
  ;; Built on top of READ-CHAR-NO-HANG because LISTEN cannot distinguish
  ;; between blocking & EOF.
  (with-accessors ((streams stream-input-streams))
      stream
    (prog ((ch nil))
     next-stream
       (cond ((endp streams)
              (return nil))
             ((eq :eof (setf ch (stream-read-char-no-hang (first streams))))
              ;; Reached end of this stream. Pop streams.
              (pop streams)
              (go next-stream))
             (ch
              (stream-unread-char (first streams) ch)
              (return t))
             (t
              (return nil))))))

(defmethod stream-clear-input ((stream concatenated-stream))
  (concatenated-stream-call-method stream stream-clear-input nil))

(defmethod stream-element-type ((stream concatenated-stream))
  (concatenated-stream-call-method stream stream-element-type nil))

(defmethod (setf stream-element-type) (new-value (stream concatenated-stream))
  (with-accessors ((streams stream-input-streams))
      stream
    (when streams
      (setf (stream-element-type (first streams)) new-value))
    new-value))

(defmethod stream-external-format ((stream concatenated-stream))
  (concatenated-stream-call-method stream stream-external-format nil))

(defmethod (setf stream-external-format) (new-value (stream concatenated-stream))
  (with-accessors ((streams stream-input-streams))
      stream
    (when streams
      (setf (stream-external-format (first streams)) new-value))
    new-value))

(defmethod stream-read-sequence ((stream concatenated-stream) seq &optional (start 0) end)
  (setf end (or end (length seq)))
  (with-accessors ((streams stream-input-streams))
      stream
    (prog ((next start))
     next
       (when (>= start end)
         (return end))
       (when (endp streams)
         (return start))
       (when (eql start (setf next (stream-read-sequence (first streams)
                                                         seq start end)))
         ;; Reached end of this stream. Pop streams.
         (pop streams))
       (setf start next)
       (go next))))

(defmethod interactive-stream-p ((stream concatenated-stream))
  (concatenated-stream-call-method stream interactive-stream-p nil))
