(in-package :xcb)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *compile-checked* t)
  (defparameter *xbug* nil)
  (defparameter *q* *standard-output*)
  (defparameter *show-unhandled-events* nil))

(defmacro xbug (&rest rest)
  (when *xbug*
    `(progn
       (format *q* "xcb:" )
       (format *q* ,@rest))))


;; Compile a checked version of the function form.
(defmacro check (function-form &key message)
  (if *compile-checked*
      (let ((checked-function-symbol
	     (intern (concatenate 'string
				  (symbol-name (car function-form))
				  "-CHECKED"))))
	(setf message (concatenate 'string "XCB fail: "
				   (symbol-name checked-function-symbol)
				   message))
	`(let ((cookie (,checked-function-symbol ,@(cdr function-form))))
	   (unless (null-pointer-p (request-check ,(cadr function-form) cookie))
	     (error ,message))
	   cookie))
      function-form))


(defmacro mvbind (vars value-form &body body)
  `(multiple-value-bind ,vars ,value-form ,@body))

(defmacro mvsetq (vars value-form &body body)
  `(multiple-value-setq ,vars ,value-form ,@body))

(defmacro mvcall (vars value-form &body body)
  `(multiple-value-call ,vars ,value-form ,@body))

(defmacro xdefun (name &body body)
  `(progn
     (defun ,name ,@body)
     (export ',name)))
(defmacro xdefconstant (name &rest rest)
  `(progn
     (defconstant ,name ,@rest)
     (export ',name)))
(defmacro xdefcfun ((cname lname) &body body)
  `(progn
     (defcfun (,cname ,lname) ,@body)
     (export ',lname)))

(defmacro xdefcstruct (name &body body)
  (let ((member-names (mapcar #'car body)))
    `(progn
       (defcstruct ,name ,@body)
       (export ,member-names))))


;;------------------------------------------------------------------------------
;; defxcb - a macro that creates both checked and unchecked bindings for xcb.
;;
;; All xcb-style functions return a cookie.
;; All xcb functions take connection as the first parameter.
;;
(defmacro defxcb ((cname lispname) &body body)
  (let ((cname-checked (concatenate 'string cname "_checked"))
	(lispname-checked (intern (concatenate 'string (symbol-name lispname) "-CHECKED"))))
    `(progn
       (defcfun (,cname ,lispname) :uint32
	 (conn        :pointer)
	 ,@body)
       (defcfun (,cname-checked ,lispname-checked) :uint32
	 (conn        :pointer)
	 ,@body))))


;;------------------
;; values
(defmacro foreign-values (&rest values)
  (multiple-value-bind (ts cts vals) (cffi::parse-args-and-types values)
    (declare (ignore ts))
    (multiple-value-bind (offsets total)
	(loop for type in cts
	   for size = (foreign-type-size type)
	   summing size into total
	   collect total into offsets
	   finally (return (values (cons 0 offsets) total)))
      `(let ((temp (cffi-sys::%foreign-alloc ,total)))
	 ,@(loop for type in cts
	      for val in vals
	      for offset in offsets
	      collect `(setf (mem-ref temp ,type ,offset) ,val))
	 temp))))

(defmacro w-foreign-values ((var &rest values) &body body)
  `(let ((,var (foreign-values ,@values)))
     ,@body
     (foreign-free ,var)))


#||
(defmacro with-foreign-values ((var &rest values) &body body)
  (multiple-value-bind (ts cts vals) (cffi::parse-args-and-types values)
    (declare (ignore ts))
    (let ((size (loop for type in cts sum (foreign-type-size type))))
      `(let ((,var (cffi-sys::%foreign-alloc ,size)))
	 ,@(loop for type in cts
	     for val in vals
	     for offset = 0 then (+ offset (foreign-type-size type))
	     collect `(setf (mem-ref ,var ,type ,offset) ,val))
	 ,@body
	 (foreign-free ,var)))))
||#

(defun grinc (string)
  (uiop:run-program (list "grep" "-r" string "/usr/include/") :output :interactive :ignore-error-status t))

;; Create an environment with generated id
(defmacro with-ids (vars &body body)
  (let ((lets (mapcar (lambda (v) `(,v (generate-id conn))) vars)))
    `(let* ((conn *conn*)
	    ,@lets)
       ,@body)))

(defun group (source n)
  "This takes a  flat list and emit a list of lists, each n long
   containing the elements of the original list"
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n)
                                   acc))
                   (nreverse (cons source acc))))))
    (if source
        (rec source nil)
        nil)))

(defun dump (pointer &optional (size-in-bytes 64))
  (let* ((size (if (oddp size-in-bytes) (1+ size-in-bytes) size-in-bytes))
         (data (loop :for i :below size :collect
                  (cffi:mem-ref pointer :uchar i)))
         (batched (group data 16))
         (batched-chars (mapcar
                         (lambda (x)
                           (mapcar
                            (lambda (c)
                              (if (and (> c 31) (< c 126))
                                  (code-char c)
                                  #\.))
                            x))
                         batched)))
    (loop :for batch :in batched :for chars :in batched-chars
       :for i :from 0 :by 16 :do
       (when (= 0 (mod i 256))
         (format t "~%87654321    0011 2233 4455 6677 8899 aabb ccdd eeff    0123456789abcdef~%")
         (format t "-----------------------------------------------------------------------~%"))
       (format t "~8,'0X    ~{~@[~2,'0X~]~@[~2,'0X ~]~}   " i batch)
       (format t "~{~a~}~{~c~}~%"
               (loop :for i :below (max 0 (floor (/ (- 16 (length batch)) 2)))
                  :collect "     ")
               chars))))

;;=============================================================================
;; atom
;;
(defun easy-atom (conn name &optional only-if-exists)
  (let ((atom-cookie
	 (xcb::intern-atom conn (if only-if-exists 1 0)
			   (length name)
			   name)))
    (when atom-cookie
      (let ((reply (xcb::intern-atom-reply conn atom-cookie
					   (null-pointer))))
	(prog1
	    (mem-ref reply :uint 8)
	  (foreign-free reply))))))

(deftype U64    () '(unsigned-byte 64))
(deftype S64    () '(signed-byte 64))
(deftype U62    () '(unsigned-byte 62))
(deftype U50    () '(unsigned-byte 50))
(deftype S50    () '(signed-byte 50))
(deftype U32    () '(unsigned-byte 32))
(deftype S32    () '(signed-byte 32))
(deftype U29    () '(unsigned-byte 29))
(deftype U24    () '(unsigned-byte 24))
(deftype U16    () '(unsigned-byte 16))
(deftype S16    () '(signed-byte 16))
(deftype U14    () '(unsigned-byte 14))
(deftype U8     () '(unsigned-byte 8))
(deftype S8     () '(signed-byte 8))
(deftype U6     () '(unsigned-byte 8))
(deftype U5     () '(unsigned-byte 5)) 
(deftype U2     () '(unsigned-byte 2))
(deftype U1     () '(unsigned-byte 1))
