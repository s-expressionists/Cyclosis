(cl:in-package #:cyclosis)

;;; Default Gray methods

(defmethod streamp (stream)
  (declare (ignore stream))
  nil)

(defmethod streamp ((stream stream))
  (declare (ignore stream))
  t)

(defmethod stream-line-column ((stream fundamental-character-output-stream))
  nil)

(defmethod stream-line-length ((stream fundamental-character-output-stream))
  nil)

(defmethod stream-read-sequence
    ((stream fundamental-input-stream) sequence &optional (start 0) end)
  (setf end (or end (length sequence)))
  (let ((n (- end start)))
    ;; For bivalent streams, perform character reads unless reading
    ;; into an integer vector.
    (if (and (subtypep (stream-element-type stream) 'character)
             (not (and (vectorp sequence)
                       (subtypep (array-element-type sequence) 'integer))))
        (dotimes (i n end)
          (let ((elt (stream-read-char stream)))
            (if (eq elt :eof)
                (return (+ start i))
                (setf (elt sequence (+ start i)) elt))))
        (dotimes (i n end)
          (let ((elt (stream-read-byte stream)))
            (if (eq elt :eof)
                (return (+ start i))
                (setf (elt sequence (+ start i)) elt)))))))

(defmethod stream-write-sequence
    ((stream fundamental-output-stream) sequence &optional (start 0) end)
  (setf end (or end (length sequence)))
  (let ((n (- end start)))
    (if (and (subtypep (stream-element-type stream) 'character)
             (not (and (vectorp sequence)
                       (subtypep (array-element-type sequence) 'integer))))
        (dotimes (i n)
          (stream-write-char stream (elt sequence (+ start i))))
        (dotimes (i n)
          (stream-write-byte stream (elt sequence (+ start i))))))
  sequence)

(defmethod close ((stream fundamental-stream) &key abort)
  (declare (ignore abort))
  (setf (stream-open-p stream) nil)
  t)

(defmethod open-stream-p ((stream t))
  (error 'type-error :expected-type 'stream :datum stream))

(defmethod open-stream-p ((stream fundamental-stream))
  (stream-open-p stream))

(defmethod input-stream-p ((stream t))
  (error 'type-error :expected-type 'stream :datum stream))

(defmethod input-stream-p ((stream fundamental-stream))
  nil)

(defmethod input-stream-p ((stream fundamental-input-stream))
  t)

(defmethod output-stream-p ((stream t))
  (error 'type-error :expected-type 'stream :datum stream))

(defmethod output-stream-p ((stream fundamental-stream))
  nil)

(defmethod output-stream-p ((stream fundamental-output-stream))
  t)

(defmethod interactive-stream-p ((stream t))
  (error 'type-error :expected-type 'stream :datum stream))

(defmethod interactive-stream-p ((stream fundamental-stream))
  nil)

(defmethod stream-element-type ((stream fundamental-character-stream))
  'character)

(defgeneric external-format-string-length (external-format string))

;; TODO
;; (defmethod external-format-string-length ((external-format (eql :default)) string)
;;   (length (sys.int::encode-utf-8-string string :eol-style :lf)))

;; TODO
;; (defmethod external-format-string-length ((external-format (eql :utf-8)) string)
;;   (length (sys.int::encode-utf-8-string string :eol-style :lf)))

(defmethod stream-file-string-length
    ((stream fundamental-character-output-stream) string)
  (external-format-string-length (stream-external-format stream) string))

(defmethod stream-clear-input ((stream fundamental-input-stream))
  nil)

(defmethod stream-clear-output ((stream fundamental-output-stream))
  nil)

(defmethod stream-finish-output ((stream fundamental-output-stream))
  nil)

(defmethod stream-force-output ((stream fundamental-output-stream))
  nil)

;; TWB: This came from Mezzano. This isn't implemented like this in
;; other implementations, so I am removing them for now.
#+(or)(defmethod stream-read-byte-no-hang ((stream fundamental-binary-input-stream))
  (ecase (stream-listen-byte stream)
    ((:eof) :eof)
    ((t) (read-byte stream nil :eof))
    ((nil) nil)))

;; TWB: This came from Mezzano. This isn't implemented like this in
;; other implementations, so I am removing them for now.
#+(or)(defmethod stream-listen-byte ((stream fundamental-binary-input-stream))
  t)

(defmethod stream-peek-char ((stream fundamental-character-input-stream))
  (let ((ch (stream-read-char stream)))
    (cond ((eql ch :eof) :eof)
          (t (stream-unread-char stream ch)
             ch))))

;; TWB: This came from Mezzano. This isn't implemented like this in
;; other implementations, so I am removing them for now.
#+(or)(defmethod stream-peek-char-skip-whitespace
    ((stream fundamental-character-input-stream))
  (loop for ch = (stream-peek-char stream)
        while (equal ch #\Space)
        do (stream-read-char stream)
        finally (return ch)))

(defmethod stream-read-char-no-hang ((stream fundamental-character-input-stream))
  (stream-read-char stream))

(defmethod stream-read-line ((stream fundamental-character-input-stream))
  (loop
     with result = (make-array 120 :element-type 'character :adjustable t :fill-pointer 0)
     for c = (stream-read-char stream)
     until (or (eql c :eof)
               (eql c #\Newline))
     do (vector-push-extend c result)
     finally (return (values result (eql c :eof)))))

(defmethod stream-listen ((stream fundamental-character-input-stream))
  (let ((ch (stream-read-char-no-hang stream)))
    (cond ((or (eql ch :eof)
               (not ch))
           nil)
          (t (stream-unread-char stream ch)
             t))))

(defmethod stream-advance-to-column
    ((stream fundamental-character-output-stream) column)
  (let ((current (line-column stream)))
    (when current
      (dotimes (i (- column current))
        (stream-write-char stream #\Newline))
      t)))

(defmethod stream-fresh-line ((stream fundamental-character-output-stream))
  (cond ((stream-start-line-p stream)
         nil)
        (t
         (stream-terpri stream)
         t)))

(defmethod stream-start-line-p ((stream fundamental-character-output-stream))
  (let ((column (stream-line-column stream)))
    (and column (zerop column))))

(defmethod stream-terpri ((stream fundamental-character-output-stream))
  (stream-write-char stream #\Newline))

(defmethod stream-write-string
    ((stream fundamental-character-output-stream) string &optional (start 0) end)
  (loop for index from start below (or end (length string))
        do (stream-write-char stream (char string index)))
  string)

(defmethod stream-file-length (stream)
  (error 'type-error :datum stream :expected-type 'file-stream))

(defmethod stream-file-position ((stream fundamental-stream) &optional position-spec)
  (declare (ignore position-spec))
  nil)

(let ((func #'cl:pathname))
  (defmethod pathname (stream)
    (funcall func stream)))

(let ((func #'cl:truename))
  (defmethod truename (stream)
    (funcall func stream)))

(defmethod stream-element-type (stream)
  (check-stream stream)
  t)
