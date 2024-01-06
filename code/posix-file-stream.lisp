(cl:in-package #:cyclosis)

(defclass posix-file-stream (file-stream
                             octet-mixin
                             character-input-mixin
                             character-output-mixin
                             fundamental-stream)
  ((descriptor :accessor descriptor
               :initform -1
               :initarg :descriptor
               :type fixnum)
   (close-descriptor :reader close-descriptorp
                     :initform t
                     :initarg :close-descriptor
                     :type boolean)
   (direction :reader direction
              :initarg :direction
              :type (member :input :output :io :probe))))

(defmethod stream-read-octets ((stream posix-file-stream) octets &optional (start 0) end)
  #+sbcl (sb-posix:read (descriptor stream)
                        (sb-sys:sap+ (sb-sys:vector-sap octets)
                                     start)
                        (- (or end (length octets)) start))
  #+sicl (sicl-posix-high:read (descriptor stream)
                               octets :start start :end (or end (length octets))))

(defmethod stream-write-octets ((stream posix-file-stream) octets &optional (start 0) end)
  #+sbcl (sb-posix:write (descriptor stream)
                         (sb-sys:sap+ (sb-sys:vector-sap octets)
                                      start)
                         (- (or end (length octets)) start))
  #+sicl (sicl-posix-high:write (descriptor stream)
                                octets :start start :end (or end (length octets))))

(defmethod close ((stream posix-file-stream) &key abort)
  (declare (ignore abort))
  (when (close-descriptorp stream)
    #+sbcl (sb-posix:close (descriptor stream))
    #+sicl (sicl-posix-low:close (descriptor stream)))
  (setf (descriptor stream) -1)
  (call-next-method))

(defmethod input-stream-p ((stream posix-file-stream))
  (and (member (direction stream) '(:input :io))
       t))

(defmethod output-stream-p ((stream posix-file-stream))
  (and (member (direction stream) '(:output :io))
       t))
