(in-package :webinspect)

(declaim (optimize (debug 3) (safety 3)))

(defclass acceptor (hunchentoot:acceptor)
  ((dispatch-table
    :initarg :dispatch-table
    :accessor dispatch-table
    :documentation "list of dispatch functions")
   (object
    :initarg :object
    :accessor object
    :documentation "the object to serve on the server root"))
  (:default-initargs
   :address "127.0.0.1"))

(defmethod hunchentoot:acceptor-dispatch-request ((a acceptor) req)
  (mapcar
   (lambda (d)
     (let ((handler (funcall d req)))
       (when handler
	 (return-from hunchentoot:acceptor-dispatch-request (funcall handler req)))))
   (dispatch-table a))
  (call-next-method))

(defgeneric object-summary-html (o req prefix stream))

(defmethod object-summary-html (o req prefix stream)
  (cl-who:with-html-output (stream)
    (who:htm (:a :href prefix
		 (who:str (who:escape-string-iso-8859-1 (format nil "~A" o)))))))

(defgeneric object-html (o req prefix path stream))

(defun find-slot (o slot-str)
  (loop for slot in (sb-mop:class-slots (class-of o))
	when (equal (string (sb-mop:slot-definition-name slot)) (string-upcase slot-str))
	  return (slot-value o (sb-mop:slot-definition-name slot))))

(defmethod object-html (o req prefix path stream)
  (if (not (null path))
      (let ((sub-obj (find-slot o (car path))))
	(object-html sub-obj req (join-path prefix (car path)) (cdr path) stream))
      (cl-who:with-html-output (stream)
	(who:htm
	 (:div
	  (:p "Object of Type: "
	      (who:str (who:escape-string-iso-8859-1 (format nil "~A" (type-of o)))))
	  (:p "Slots: "
	      (let ((slots (sb-mop:class-slots (class-of o))))
		(mapcar
		 (lambda (slot)
		   (let ((name (sb-mop:slot-definition-name slot)))
		     (who:htm 
		      (:div
		       (:p (who:str name) " : "
			   (when (slot-boundp o name)
			     (object-summary-html
			      (slot-value o name)
			      req
			      (join-path prefix name)
			      stream)))))))
		 slots))))))))

(defmethod object-html ((o cons) req prefix path stream)
  (if (not (null path))
      (let* ((i (parse-integer (car path)))
	     (sub-obj (nth i o)))
	(object-html sub-obj req (join-path prefix (car path)) (cdr path) stream))
      (cl-who:with-html-output (stream)
	(who:htm
	 (:div (:p (who:str (format nil "CONS OF LENGTH ~A" (length o))))
	       (:div 
		(loop for obj in o
		      for i from 0
		      do (who:htm
			  (:p (object-summary-html obj req (join-path prefix i) stream))))))))))

(defun strip-ending-slash (path)
  (if (equal (subseq path (1- (length path))) "/")
      (subseq path 0 (1- (length path)))
      path))

(defun strip-leading-slash (path)
  (if (equal (subseq path 0 1) "/")
      (subseq path 1)
      path))

(defun stringify (o)
  (if (stringp o)
      o
      (format nil "~A" o)))

(defun join-path (prefix end)
;  (break)
  ;(if (equal (subseq prefix (1- (length prefix))) "/")
  ;    (format nil "~A~A" prefix end)
					;    (format nil "~A/~A" prefix end)))
  (format nil "~A/~A"
	  (strip-ending-slash (stringify prefix))
	  (strip-leading-slash (stringify end))))

(defun direct-path (path)
  (join-path "/direct/"
	     (format nil "~{~A~^/~}"
		     (cdr (split-path path)))))

(defgeneric object-direct (o req prefix path))

(defmethod object-direct (o req prefix path)
  (format t "OBJECT DIRECT GENERIC~%")
  (if (not (null path))
      (let ((sub-obj (find-slot o (car path))))
	(object-direct sub-obj req (join-path prefix (car path)) (cdr path)))
      (progn
	(setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
	"")))

(defmethod object-direct ((o cons) req prefix path)
  (format t "OBJECT DIRECT CONS~%")
  (if (not (null path))
      (let* ((i (parse-integer (car path)))
	     (sub-obj (nth i o)))
	(object-direct sub-obj req (join-path prefix (car path)) (cdr path)))
      (progn
	(setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
	"")))

(defun split-path (path)
  (remove-if (lambda (x) (equal x ""))
	     (uiop:split-string path :separator "/")))

(defun make-root-handler (a)
  (declare (type acceptor a))
  (lambda (req)
    (let ((s (flex:make-flexi-stream (hunchentoot:send-headers)))
	  (path (cdr
		 (remove-if (lambda (x) (equal x ""))
			    (uiop:split-string (hunchentoot:script-name req) :separator "/")))))
      (object-html (object a) req "/object" path s))))

(defun make-direct-handler (a)
  (declare (type acceptor a))
  (lambda (req)
    (let ((path (cdr
		 (remove-if (lambda (x) (equal x ""))
			    (uiop:split-string (hunchentoot:script-name req) :separator "/")))))
      (object-direct (object a) req "/direct" path))))

(defun make-inspector-server (object &key (port 8082))
  (let ((a (make-instance 'acceptor :object object :port port)))
    (setf (dispatch-table a)
	  (list (hunchentoot:create-prefix-dispatcher "/direct" (make-direct-handler a))))
    (push (hunchentoot:create-prefix-dispatcher "/object" (make-root-handler a)) (dispatch-table a))
    a))
