(cl:in-package #:cyclosis)

(defclass utf-8-transcoder (character-transcoder) ())

(defmethod read-element ((transcoder utf-8-transcoder) stream)
  (let (octet1 octet2 octet3 octet4)
    (cond ((eq (setf octet1 (stream-read-octet stream)) :eof)
           :eof)
          ((< octet1 #b10000000)
           (coerce (code-char octet1) (stream-element-type stream)))
          ((eq (setf octet2 (stream-read-octet stream)) :eof)
           (unexpected-eof transcoder stream octet1))
          ((not (<= #b10000000 octet2 #b10111111))
           (illegal-sequence transcoder stream octet1 octet2))
          ((< octet1 #b11100000)
           (coerce (code-char (+ (ash (- octet1 #b11000000) -2)
                                 (- octet2 #b10000000)))
                   (stream-element-type stream)))
          ((eq (setf octet3 (stream-read-octet stream)) :eof)
           (unexpected-eof transcoder stream octet1 octet2))
          ((not (<= #b10000000 octet3 #b10111111))
           (illegal-sequence transcoder stream octet1 octet2 octet3))
          ((< octet1 #b11110000)
           (coerce (code-char (+ (ash (- octet1 #b11100000) -4)
                                 (ash (- octet2 #b10000000) -2)
                                 (- octet3 #b10000000)))
                   (stream-element-type stream)))
          ((eq (setf octet4 (stream-read-octet stream)) :eof)
           (unexpected-eof transcoder stream octet1 octet2 octet3))
          ((not (and (<= #b10000000 octet4 #b10111111)
                     (< octet1 #b11111000)))
           (illegal-sequence transcoder stream octet1 octet2 octet3 octet4))
          (t
           (coerce (code-char (+ (ash (- octet1 #b11110000) -6)
                                 (ash (- octet2 #b10000000) -4)
                                 (ash (- octet3 #b10000000) -2)
                                 (- octet4 #b10000000)))
                   (stream-element-type stream))))))

(defmethod write-element ((transcoder utf-8-transcoder) stream element)
  (let ((code (char-code element))
        (m1 #b10000000)
        (m2 #b00111111))
    (cond ((< code #x80)
           (stream-write-octet stream code))
          ((< code #x800)
           (stream-write-octet stream (logior #b11000000 (ash code -6)))
           (stream-write-octet stream (logior m1 (logand code m2))))
          ((< code #x1000)
           (stream-write-octet stream (logior #b11100000 (ash code -12)))
           (stream-write-octet stream (logior m1 (logand (ash code -6) m2)))
           (stream-write-octet stream (logior m1 (logand code m2))))
          (t
           (stream-write-octet stream (logior #b11110000 (ash code -18)))
           (stream-write-octet stream (logior m1 (logand (ash code -12) m2)))
           (stream-write-octet stream (logior m1 (logand (ash code -6) m2)))
           (stream-write-octet stream (logior m1 (logand code m2))))))
  element)

(defmethod encoded-length ((transcoder utf-8-transcoder) elements)
  (reduce (lambda (previous element
                   &aux (code (char-code element)))
            (+ previous
               (cond ((< code #x80) 1)
                     ((< code #x800) 2)
                     ((< code #x1000) 3)
                     (t 4))))
          elements
          :initial-value 0))

(defmethod make-transcoder
    ((external-format (eql :utf-8)) element-type &rest options)
  (apply #'make-instance 'utf-8-transcoder options))
