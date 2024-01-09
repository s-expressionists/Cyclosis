(cl:in-package #:cyclosis)

(defclass character-transcoder (transcoder)
  ((eol :reader eol
        :initform #+windows :crlf #-windows :lf
        :initarg :eol
        :type (member :lf :cr :crlf))))

(defmethod read-element :around ((transcoder character-transcoder) stream)
  (case (eol transcoder)
    (:cr
     (let ((ch (call-next-method)))
       (if (eql ch #\return)
           #\newline
           ch)))
    (:crlf
     (let ((ch (call-next-method)))
       (if (eql ch #\return)
           (let ((ch2 (call-next-method)))
             (cond ((eq ch2 :eof)
                    (unexpected-eof transcoder stream ch))
                   ((eql ch2 #\linefeed)
                    #\newline)
                   (t
                    (illegal-sequence transcoder stream ch ch2))))
           ch)))
    (otherwise
     (let ((ch (call-next-method)))
       (if (eql ch #\linefeed)
           #\newline
           ch)))))

(defmethod write-element :around ((transcoder character-transcoder) stream element)
  (if (eql element #\newline)
      (case (eol transcoder)
        (:cr
         (call-next-method transcoder stream #\return))
        (:crlf
         (call-next-method transcoder stream #\return)
         (call-next-method transcoder stream #\linefeed))
        (otherwise
         (call-next-method transcoder stream #\linefeed)))
      (call-next-method)))
