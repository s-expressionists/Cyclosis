(cl:in-package #:cyclosis)

;;; Concatenated stream.

(defclass concatenated-stream (fundamental-character-input-stream ; For the default stream-read-line method.
                               fundamental-input-stream
                               unread-char-mixin)
  ((streams :initarg :streams
            :reader concatenated-stream-streams
            :accessor %streams)))

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
       (pop (slot-value stream 'streams)))))

(defmethod stream-read-char ((stream concatenated-stream))
  (loop
     (when (endp (concatenated-stream-streams stream))
       (return :eof))
     (let ((ch (stream-read-char (first (concatenated-stream-streams stream)))))
       (when (not (eql ch :eof))
         (return ch))
       ;; Reached end of this stream. Pop streams.
       (pop (slot-value stream 'streams)))))

(defmethod stream-read-char-no-hang ((stream concatenated-stream))
  (loop
     (when (endp (concatenated-stream-streams stream))
       (return :eof))
     (let ((ch (stream-read-char-no-hang (first (concatenated-stream-streams stream)))))
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
     (let ((ch (stream-read-char-no-hang (first (concatenated-stream-streams stream)))))
       (case ch
         (:eof
          ;; Reached end of this stream. Pop streams.
          (pop (slot-value stream 'streams)))
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
  (loop
     (when (endp (concatenated-stream-streams stream))
       (return :eof))
     (let ((ch (stream-peek-char (first (concatenated-stream-streams stream)))))
       (when (not (eql ch :eof))
         (return ch))
       ;; Reached end of this stream. Pop streams.
       (pop (slot-value stream 'streams)))))
