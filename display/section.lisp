(in-package :xcb)

;;==============================================================================
;;
;; SEC - a section of text, an independent unit of display.
;;
(defstruct (sec (:constructor make-sec%))
  ;; foreign buffer with sequential xbufs (see xbuf.lisp)
  (buf nil :type foreign-pointer)
  (ptr nil :type foreign-pointer) ;; write pointer
  ;; Lisp array of objects referred to by xbufs
  (chunks nil :type simple-vector)
  (chindex 0  :type U32)
  ;; spacing support
  (halfspace 0 :type U32)
  ;; layout support
  (indent 0  :type U32)
  (lines nil :type simple-vector)
  (lindex 0  :type U32)
  ;; geometry
  (margin-right 400  :type U32)
  (margin-left  10   :type U32)
  (margin-top   10   :type U32)
  (margin-bottom 280  :type U32)
  )
(defmacro in-sec ((sec) &body body)
  `(let ((sec ,sec))
     (with-accessors ((buf. sec-buf)
		      (ptr. sec-ptr)
		      (chunks. sec-chunks)
		      (chindex. sec-chindex)
		      (halfspace. sec-halfspace)
		      (indent. sec-indent)
		      (lines. sec-lines)
		      (lindex. sec-lindex)
		      (margin-r. sec-margin-right) (margin-l. sec-margin-left)
		      (margin-t. sec-margin-top) (margin-b. sec-margin-bottom)
		      
		      ) sec
       ,@body)))
;; -----------------------------------------------------------------------------
(defun make-sec (&key (buffer-size #x10000) (array-size 1024) )
  (let ((buf (foreign-alloc :char :count buffer-size))
	(arr (make-array array-size :element-type t)))
  ;;  (setf (aref arr 0) *the-cond-space*)
    (make-sec% :buf buf
		  :ptr (inc-pointer buf +xbuf-prefix+)
		  :chunks arr
		  :chindex 0
		  :lines (make-array 128 :element-type t ))))
;; -----------------------------------------------------------------------------
(defmethod print-object ((o sec) s)
  (print-unreadable-object (o s :type t )
    (loop for i below (sec-chindex o)
       do (princ (aref (sec-chunks o) i) s))))
;; -----------------------------------------------------------------------------
(defun sec-append-prim (sec obj string flags styndex)
  (with-accessors ((chunks. sec-chunks) (index. sec-chindex)
		   (ptr. sec-ptr)) sec
    (let ((index index.))
      (setf (aref chunks. index) obj) ;; store object in our lisp array;
      ;; write xbuf, and update ptr
      (setf ptr. (xbuf-set ptr. index string flags styndex))
      ;; store 0-length at ptr to indicate final object- for now...TODO:
      (setf (xbuf-data-length ptr.) 0)
      (incf index.)
      obj)))
;; -----------------------------------------------------------------------------
(defparameter *sec* (make-sec)
  )
;; -----------------------------------------------------------------------------

(defstruct displayable )
(defmethod display-as ((obj displayable) sec)
  )


(defstruct (disp-space (:include displayable)))
(defmethod print-object ((o disp-space) s)
  (print-unreadable-object (o s  )
    (princ " " s)
))

(defparameter *the-space* (make-disp-space))
(defmethod sec-append (sec (obj displayable) cons)
  (sec-append-prim sec *the-space* " " +KIND-NORMAL+ style-space)
  )


;; -----------------------------------------------------------------------------
;; half-space
(defmacro sec-incf-halfspace (sec)
  `(incf (sec-halfspace ,sec)))
(defmacro sec-clear-halfspace (sec)
  `(setf (sec-halfspace ,sec) 0))
(defmacro sec-set-halfspace (sec)
  `(setf (sec-halfspace ,sec) 1))

(defun sec-maybe-space (sec)
  (when (> (sec-incf-halfspace sec) 1)
    (sec-append sec *the-space* nil)
    (sec-clear-halfspace sec)))
;;====================================================================
;; Append an object to display list of sec.
;;
;; To facilitate editing of structures, we store the cons if possible.
;; If not, the object is not editable or updatable.
(defmethod sec-append (sec (obj symbol) cons)
  (sec-maybe-space sec)
  (sec-append-prim sec cons (string-downcase (symbol-name obj))
		      +KIND-NORMAL+ style-symbol)
  (sec-set-halfspace sec))

(defmethod sec-append (sec (obj string) cons)
  (sec-maybe-space sec)
  (sec-append-prim sec obj "\""  +KIND-NORMAL+ style-string)
  (sec-append-prim sec cons obj  +KIND-NORMAL+ style-string)
  (sec-append-prim sec obj "\""  +KIND-NORMAL+ style-string)
  (sec-set-halfspace sec))

(defmethod sec-append (sec (obj number) cons)
  (sec-maybe-space sec)
  (sec-append-prim sec cons (format nil "~A" obj)  +KIND-NORMAL+ style-literal)
  (sec-set-halfspace sec))

(defmethod sec-append (sec (obj list) cons)
  (sec-maybe-space sec)
  (sec-append-prim  sec obj "(" +KIND-OPEN+ style-paren)
  (sec-clear-halfspace sec)
   (loop for cons on obj do
	(sec-append sec (car cons) cons))
   (sec-append-prim  sec obj ")" +KIND-CLOSE+ style-paren)
  (sec-set-halfspace sec))

;; Layout the sec into lines.
;; Note: break-it comes in two stages: first break-it triggers a newline
;; and another check; second break-it on a new line means we really need to
;; break it.
(let (closer)
  ;;==============================================
;;
;; output a simple one, and return next x and
  (defun sec-layout-simple (ptr x)
    (let ((len (xbuf-data-length ptr)))
      (loop for i below len do
	   (princ (code-char (xbuf-ref ptr i))) ;;TODO: this is for test output
	   (setf (xbuf-x ptr) x))
      (values (xbuf-next ptr) (+ x (xbuf-pix-width ptr)))))
  
  ;;==============================================================================
  ;; layout-expr
  ;;
  ;; Layout an expression in this line...unconditionally, returning x and next-ptr.
  ;;
  (defun sec-layout-exp(ptr x)
    ;; (format t "~%sec-layout-exp ~A ~A" ptr x)
    (prog ()
       (mvsetq (ptr x) (sec-layout-simple ptr x)); we _know_ it is a (
       again
       (let ((kind  (xbuf-kind ptr)))
	 (case kind
	   (#.+KIND-OPEN+   (mvsetq (ptr x)(sec-layout-exp ptr x)))
	   (#.+KIND-NORMAL+ (mvsetq (ptr x)(sec-layout-simple ptr x)))
	   (#.+KIND-CLOSE+  (push ptr closer) (setf ptr (xbuf-next ptr))))
	 (unless (= kind +KIND-CLOSE+)
	   (go again)))
       (return (values ptr x ))))
  
  
  ;;------------------------------------------------------------------------------
  ;; Attempt to process an item testing for fit; return t x ptr or nil
  ;; simple items get an :it-fits or :wrap-it; ( is processed as a unit and
  ;; gets an :it-fits or :break-it.  To process :break-it try it on a new line
  ;; first, and it it's still break-it, process ( unconditionally on a new line,
  ;; then just continue.
  (defun sec-layout-try-simple (sec ptr x)
    (in-sec (sec)
      (mvbind (proposed-ptr proposed-x) (sec-layout-simple ptr x)
	(when (< proposed-x margin-r.)
	  (values proposed-ptr proposed-x )))))
  
  (defun sec-layout-try-exp (sec ptr x)
    (in-sec (sec)
      (mvbind (proposed-ptr proposed-x) (sec-layout-exp ptr x)
	(when (< proposed-x margin-r.)
	  (values proposed-ptr proposed-x )))))
  
  
  
  (defun sec-layout (sec ptr x indent)
    ;; (format t "~%---~A" indent)
    (in-sec (sec)
      (labels ((cr ()
		 (loop while closer do
		      (mvsetq (ptr x)(sec-layout-simple (pop closer) x )))
		 (terpri)
		 (setf x indent)
		 ))
	;; process opener
	(mvsetq (ptr x) (sec-layout-simple ptr x))
	(prog ((p ptr))
	   ;;-------------------------------------------
	 again
	 (when (plusp (xbuf-data-length p))
	   (case (xbuf-kind p)
	     (#.+KIND-NORMAL+ (mvbind (pp px) (sec-layout-try-simple sec p x)
				(if pp (setf p pp  x px)
				    ;; either :wrap-it or :break-it requires a new line.
				    (progn ; if it does not fit, go to next line
				      (terpri)    ;; TODO: increment y
				      (setf x indent) ))))
	     (#.+KIND-OPEN+
	      (format t "|IN|")
	      (let ((closer1 closer))
		(mvbind (pp px) (sec-layout-try-exp sec p x)
		  (if pp
		      (progn
			(format t "|YES|")
			(setf p pp  x px) ;; processed in entirety!
			(cr)
			;;		    (format t "|TOTS|")
			)
		      ;; either :wrap-it or :break-it requires a new line.
		      (progn ; if it does not fit, go to next line
			(format t "|NO|")
			(setf closer closer1)
			(cr)    ;; TODO: increment y
			(mvsetq (p x) (sec-layout sec p x x))
			;;		    (format t "|PART ~A|" pp )
			)))))
	     (#.+KIND-CLOSE+
	      (push p closer)
	      (format t "|CLOSER|")
	      (return-from sec-layout (values p x))))
	   (go again))
	 
	 )))))


(defun sec-layout-in (sec)
  (in-sec (sec)
    (setf (xbuf-flags ptr.) 0) ;;terminate
    (sec-layout sec (inc-pointer buf. +xbuf-prefix+) 0 0 )))


(defun ttt ()
  (setf *sec* (make-sec))
  (sec-append *sec* '(defmethod sec-append (sec (obj list) cons)
			    (sec-maybe-space sec)
			    (sec-append-prim
			     sec obj "("
			     0 0 (pen-pic *pen-white*) (pen-pic *pen-black*) )
			    (sec-clear-halfspace sec)
			    (loop for cons on obj do
				 (sec-append sec (car cons) cons))
			    (sec-append-prim
			     sec obj ")"
			     0 0 (pen-pic *pen-white*) (pen-pic *pen-black*) )
			    (sec-set-halfspace sec)) nil))
