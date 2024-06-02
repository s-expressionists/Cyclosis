(cl:in-package #:cyclosis)

(defclass transcoder ()
  ((replacement :reader replacement
                :initform nil
                :initarg :replacement)))

(defun illegal-sequence (transcoder stream &rest octets)
  (or (replacement transcoder)
      (error 'illegal-sequence :stream stream :octets octets)))

(defun unexpected-eof (transcoder stream &rest octets)
  (or (replacement transcoder)
      (error 'unexpected-eof :stream stream :octets octets)))

(defun invalid-element (transcoder stream element)
  (or (replacement transcoder)
      (error 'invalid-element :stream stream :element element)))

(defgeneric encode-table (transcoder))

(defgeneric decode-table (transcoder))

(defclass octet-transcoder (transcoder)
  ())

(defmethod read-element ((transcoder octet-transcoder) stream)
  (let ((octet (stream-read-octet stream)))
    (if (eq octet :eof)
        :eof
        (multiple-value-bind (element presentp)
            (gethash octet (decode-table transcoder))
          (if presetp
              (coerce element (stream-element-type stream))
              (illegal-sequence transcoder stream octet))))))

(defmethod write-element ((transcoder octet-transcoder) stream element)
  (multiple-value-bind (octet presentp)
      (gethash element (encode-table transcoder))
    (if presentp
        (stream-write-octet stream octet)
        (invalid-element transcoder stream element)))
  element)

(defmethod encoded-length ((transcoder octet-transcoder) elements)
  (length elements))

(defmacro define-octet-transcoder (name key &rest mapping)
  `(progn
     (defclass ,name (octet-transcoder)
       ((encode-table :accessor encode-table
                      :initform (loop with table = (make-hash-table)
                                      for (octet-start octet-end code-start code-end) in ',mapping
                                      finally (return table)
                                      when code-start
                                        do (loop for octet from octet-start upto octet-end
                                                 for code from code-start upto code-end
                                                 do (setf (gethash (code-char code) table) octet))
                                      else
                                        do (setf (gethash (code-char octet-end) table) octet-start))
                      :allocation :class
                      :type hash-table)
        (decode-table :accessor decode-table
                      :initform (loop with table = (make-hash-table)
                                      for (octet-start octet-end code-start code-end) in ',mapping
                                      finally (return table)
                                      when code-start
                                        do (loop for octet from octet-start upto octet-end
                                                 for code from code-start upto code-end
                                                 do (setf (gethash octet table) (code-char code)))
                                      else
                                        do (setf (gethash octet-start table) (code-char octet-end)))
                      :allocation :class
                      :type hash-table)))

     (defmethod make-transcoder
         ((external-format (eql ,key)) element-type &rest options)
       (apply #'make-instance ',name options))))
