(cl:in-package #:cyclosis)

(defclass posix-file-stream (file-stream
                             octet-mixin
                             character-input-mixin
                             character-output-mixin
                             fundamental-character-input-stream
                             fundamental-character-output-stream
                             fundamental-binary-input-stream
                             fundamental-binary-output-stream)
  ((descriptor :accessor descriptor
               :initform -1
               :initarg :descriptor
               :type fixnum)
   (close-descriptor :reader close-descriptorp
                     :initform t
                     :initarg :close-descriptor
                     :type boolean)
   (input :reader input-stream-p
          :initform nil
          :initarg :input
          :type boolean)
   (output :reader output-stream-p
           :initform nil
           :initarg :output
           :type boolean)
   (%pathname :reader pathname
              :initform nil
              :initarg :pathname
              :type (or null pathname))
   (temp-pathname :accessor temp-pathname
                  :initform nil
                  :initarg :temp-pathname
                  :type (or null pathname))
   (created :reader created
            :initform nil
            :initarg :created
            :type boolean)))

(defmethod truename ((stream posix-file-stream))
  (let ((path (or (temp-pathname stream) (%pathname stream))))
    (if path
        (truename path)
        nil)))

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

(defmethod stream-file-position ((stream posix-file-stream) &optional position)
  #+sbcl
  (case position
    (:start
     (sb-posix:lseek (descriptor stream) 0 sb-posix:seek-set))
    (:end
     (sb-posix:lseek (descriptor stream) 0 sb-posix:seek-end))
    ((nil)
     (sb-posix:lseek (descriptor stream) 0 sb-posix:seek-cur))
    (otherwise
     (sb-posix:lseek (descriptor stream) position sb-posix:seek-set))))

(defmethod stream-file-length ((stream posix-file-stream))
  #+sbcl
  (let ((current (sb-posix:lseek (descriptor stream) 0 sb-posix:seek-cur)))
    (prog1
        (sb-posix:lseek (descriptor stream) 0 sb-posix:seek-end)
      (sb-posix:lseek (descriptor stream) current sb-posix:seek-set))))

(defmethod close ((stream posix-file-stream) &key abort)
  (declare (ignore abort))
  (when (stream-open-p stream)
    (when (close-descriptorp stream)
      #+sbcl (sb-posix:close (descriptor stream))
      #+sicl (sicl-posix-low:close (descriptor stream)))
    (setf (descriptor stream) -1)
    (cond ((null abort)
           (when (temp-pathname stream)
             (delete-file (pathname stream))
             (rename-file (temp-pathname stream) (pathname stream))
             (setf (temp-pathname stream) nil)))
          ((created stream)
           (delete-file (pathname stream)))
          ((temp-pathname stream)
           (delete-file (temp-pathname stream))
           (setf (temp-pathname stream) nil))))
  (call-next-method))

(defun find-unique-pathname (path suffix)
  (loop for i from 1 to 9
        for new-path = (make-pathname :name (format nil "~a-~a-~a"
                                                    (pathname-name path)
                                                    suffix i)
                                      :defaults path)
        finally (error 'file-error :pathname path)
        unless (probe-file new-path)
          return new-path))

#+(or sbcl sicl)
(defmethod cyclosis:make-file-stream
    (client path direction if-exists if-does-not-exist element-type external-format)
  (declare (ignore client))
  (let* ((mode (logior sb-posix:s-irusr sb-posix:s-iwusr
                       sb-posix:s-irgrp sb-posix:s-iwgrp
                       sb-posix:s-iroth sb-posix:s-iwoth))
         (flags 0)
         (temp-path nil)
         (appending nil)
         (created nil)
         (descriptor nil)
         (name (namestring path))
         (exists #+sbcl (handler-case
                            (zerop (sb-posix:access name sb-posix:f-ok))
	                  (sb-posix:syscall-error () nil))))
    (case direction
      ((:input :probe)
       (if exists
           #+sbcl (setf descriptor (sb-posix:open name sb-posix:o-rdonly))
           (case if-does-not-exist
             (:error
              (error 'file-does-not-exist :pathname path))
             (:create
              (ensure-directories-exist name)
              (setf flags (logior sb-posix:o-creat sb-posix:o-rdonly)))
             (otherwise
              (return-from make-file-stream nil)))))
      (otherwise
       (when (and (eq if-exists :new-version) (eq if-does-not-exist :create))
         (setf exist nil))
       (setf flags (if (eq direction :io)
                       sb-posix:o-rdwr
                       sb-posix:o-wronly))
       (if exists
           (case if-exists
             (:error
              (error 'file-exists :pathname path))
             (:rename
              (rename-file path (find-unique-pathname path "bak"))
              (setf flags (logior flags sb-posix:o-creat)))
             ((:rename-and-delete :new-version :supersede)
              (setf temp-path (find-unique-pathname path "tmp")
                    name (namestring temp-path))
              (setf flags (logior flags sb-posix:o-creat)))
             (:append
              (setf appending t))
             (:overwrite)
             (otherwise
              (return-from make-file-stream nil)))
           (case if-does-not-exist
             (:error
              (error 'file-does-not-exist :pathname path))
             (:create
              (setf created t)
              (ensure-directories-exist name)
              (setf flags (logior flags sb-posix:o-creat sb-posix:o-trunc)))
           (otherwise
            (return-from make-file-stream nil))))))
    #+sbcl
    (setf descriptor
          (handler-case
              (sb-posix:open name flags mode)
            (sb-posix:syscall-error (condition)
              (error 'open-fail :pathname (pathname name)
                                :message (sb-int:strerror (sb-posix:syscall-errno condition))))))
    (let ((stream (make-instance 'posix-file-stream
                                 :descriptor descriptor
                                 :pathname path
                                 :input (and (member direction '(:input :io)) t)
                                 :output (and (member direction '(:output :io)) t)
                                 :element-type element-type
                                 :external-format external-format
                                 :temp-pathname temp-path
                                 :created created)))
      (cond ((eq direction :probe)
             (close stream))
            (appending
             (stream-file-position stream (stream-file-length stream))))
      stream)))
