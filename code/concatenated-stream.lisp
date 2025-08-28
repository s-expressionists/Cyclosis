(cl:in-package #:cyclosis)

;;; Concatenated stream.

(defclass concatenated-stream (character-input-mixin
                               fundamental-character-input-stream)
  ((streams :reader concatenated-stream-streams
            :accessor %streams
            :initarg :streams
            :type list))
  (:documentation "A concatenated stream is an input stream which is a composite stream of zero
or more other input streams, such that the sequence of data which can be read from the
concatenated stream is the same as the concatenation of the sequences of data which could be
read from each of the constituent streams."))

(defmethod initialize-instance :after ((instance concatenated-stream) &rest initargs &key)
  (declare (ignore initargs))
  (mapc #'check-input-stream (concatenated-stream-streams instance)))

(defmethod stream-element-type ((stream concatenated-stream))
  (if (concatenated-stream-streams stream)
      (stream-element-type (first (concatenated-stream-streams stream)))
      nil))

(defmethod stream-read-sequence
    ((stream concatenated-stream) seq &optional (start 0) end)
  (setf end (or end (length seq)))
  (with-accessors ((streams %streams))
      stream
    (loop
      (when (>= start end)
        (return end))
      (when (endp streams)
        (return start))
      (let ((next (stream-read-sequence (first streams)
                                        seq start end)))
        (when (eql next start)
          ;; Reached end of this stream. Pop streams.
          (pop streams))
        (setf start next)))))

(defmethod stream-read-byte ((stream concatenated-stream))
  (loop
     (when (endp (concatenated-stream-streams stream))
       (return :eof))
     (let ((ch (stream-read-byte (first (concatenated-stream-streams stream)))))
       (when (not (eql ch :eof))
         (return ch))
       ;; Reached end of this stream. Pop streams.
       (pop (%streams stream)))))

(defmethod stream-read-char ((stream concatenated-stream))
  (loop
     (when (endp (concatenated-stream-streams stream))
       (return :eof))
     (let ((ch (stream-read-char (first (concatenated-stream-streams stream)))))
       (when (not (eql ch :eof))
         (return ch))
       ;; Reached end of this stream. Pop streams.
       (pop (%streams stream)))))

(defmethod stream-read-char-no-hang ((stream concatenated-stream))
  (loop
     (when (endp (concatenated-stream-streams stream))
       (return :eof))
     (let ((ch (stream-read-char-no-hang (first (concatenated-stream-streams stream)))))
       (when (not (eql ch :eof))
         (return ch))
       ;; Reached end of this stream. Pop streams.
       (pop (%streams stream)))))

(defmethod stream-listen ((stream concatenated-stream))
  ;; Built on top of READ-CHAR-NO-HANG because LISTEN cannot distinguish
  ;; between blocking & EOF.
  (loop
     (when (endp (concatenated-stream-streams stream))
       (return nil))
     (let ((ch (stream-read-char-no-hang (first (concatenated-stream-streams stream)))))
       (case ch
         (:eof
          ;; Reached end of this stream. Pop streams.
          (pop (%streams stream)))
         (nil
          (return nil))
         (t
          (stream-unread-char (first (concatenated-stream-streams stream)) ch)
          (return t))))))

(defmethod stream-clear-input ((stream concatenated-stream))
  (if (concatenated-stream-streams stream)
      (stream-clear-input (first (concatenated-stream-streams stream)))
      nil))

(defmethod stream-peek-char ((stream concatenated-stream))
  (with-accessors ((streams %streams))
      stream
    (loop (when (endp streams)
            (return :eof))
          (let ((ch (stream-peek-char (first streams))))
            (when (not (eql ch :eof))
              (return ch))
            ;; Reached end of this stream. Pop streams.
            (pop streams)))))
