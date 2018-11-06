(in-package :xcb)

;;==============================================================================
;;
;; SEC - a section of text, an independent unit of display.
;;
(defstruct (tpanel (:include panel) (:constructor make-tpanel%))
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
  (lindex 0  :type U32))
(defmacro in-tpanel ((tpanel) &body body)
  `(let ((tpanel ,tpanel))
     (with-accessors ((buf. tpanel-buf)
		      (ptr. tpanel-ptr)
		      (chunks. tpanel-chunks)
		      (chindex. tpanel-chindex)
		      (halfspace. tpanel-halfspace)
		      (indent. tpanel-indent)
		      (lines. tpanel-lines)
		      (lindex. tpanel-lindex)) tpanel
       ,@body body)))
;; -----------------------------------------------------------------------------
(defun make-tpanel (&key (buffer-size #x10000) (array-size 1024) )
  (let ((buf (foreign-alloc :char :count buffer-size))
	(arr (make-array array-size :element-type t)))
  ;;  (setf (aref arr 0) *the-cond-space*)
    (make-tpanel% :buf buf
		  :ptr (inc-pointer buf +xbuf-prefix+)
		  :chunks arr
		  :chindex 0
		  :lines (make-array 128 :element-type t ))))
;; -----------------------------------------------------------------------------
(defmethod print-object ((o tpanel) s)
  (print-unreadable-object (o s :type t )
    (in-tpanel (o)
      (format s "(~A,~A) ~Ax~A; [~A-~A ~A|~A] ~A objects"
	      x. y. w. h.
	      ml. mr. mt. mb.
	      chindex.))
    (loop for i below (tpanel-chindex o)
       do (princ (aref (tpanel-chunks o) i) s))))
;; -----------------------------------------------------------------------------
(defun tpanel-append-prim (tpanel obj string flags styndex)
  (with-accessors ((chunks. tpanel-chunks) (index. tpanel-chindex)
		   (ptr. tpanel-ptr)) tpanel
    (let ((index index.))
      (setf (aref chunks. index) obj) ;; store object in our lisp array;
      ;; write xbuf, and update ptr
      (setf ptr. (xbuf-set ptr. index string flags styndex))
      ;; store 0-length at ptr to indicate final object- for now...TODO:
      (setf (xbuf-data-length ptr.) 0)
      (incf index.)
      obj)))
;; -----------------------------------------------------------------------------
(defparameter *tpanel* (make-tpanel)
  )
;; -----------------------------------------------------------------------------

(defstruct displayable )
(defmethod display-as ((obj displayable) tpanel)
  )


(defstruct (disp-space (:include displayable)))
(defmethod print-object ((o disp-space) s)
  (print-unreadable-object (o s  )
    (princ " " s)
))

(defparameter *the-space* (make-disp-space))
(defmethod tpanel-append (tpanel (obj displayable) cons)
  (tpanel-append-prim tpanel *the-space* " " +KIND-NORMAL+ style-space)
  )


;; -----------------------------------------------------------------------------
;; half-space
(defmacro tpanel-incf-halfspace (tpanel)
  `(incf (tpanel-halfspace ,tpanel)))
(defmacro tpanel-clear-halfspace (tpanel)
  `(setf (tpanel-halfspace ,tpanel) 0))
(defmacro tpanel-set-halfspace (tpanel)
  `(setf (tpanel-halfspace ,tpanel) 1))

(defun tpanel-maybe-space (tpanel)
  (when (> (tpanel-incf-halfspace tpanel) 1)
    (tpanel-append tpanel *the-space* nil)
    (tpanel-clear-halfspace tpanel)))
;;====================================================================
;; Append an object to display list of tpanel.
;;
;; To facilitate editing of structures, we store the cons if possible.
;; If not, the object is not editable or updatable.
(defmethod tpanel-append (tpanel (obj symbol) cons)
  (tpanel-maybe-space tpanel)
  (tpanel-append-prim tpanel cons (string-downcase (symbol-name obj))
		      +KIND-NORMAL+ style-symbol)
  (tpanel-set-halfspace tpanel))

(defmethod tpanel-append (tpanel (obj string) cons)
  (tpanel-maybe-space tpanel)
  (tpanel-append-prim tpanel obj "\""  +KIND-NORMAL+ style-string)
  (tpanel-append-prim tpanel cons obj  +KIND-NORMAL+ style-string)
  (tpanel-append-prim tpanel obj "\""  +KIND-NORMAL+ style-string)
  (tpanel-set-halfspace tpanel))

(defmethod tpanel-append (tpanel (obj number) cons)
  (tpanel-maybe-space tpanel)
  (tpanel-append-prim tpanel cons (format nil "~A" obj)  +KIND-NORMAL+ style-literal)
  (tpanel-set-halfspace tpanel))

(defmethod tpanel-append (tpanel (obj list) cons)
  (tpanel-maybe-space tpanel)
  (tpanel-append-prim  tpanel obj "(" +KIND-OPEN+ style-paren)
  (tpanel-clear-halfspace tpanel)
   (loop for cons on obj do
	(tpanel-append tpanel (car cons) cons))
   (tpanel-append-prim  tpanel obj ")" +KIND-CLOSE+ style-paren)
  (tpanel-set-halfspace tpanel))

;; Layout the tpanel into lines.
;; Note: break-it comes in two stages: first break-it triggers a newline
;; and another check; tpanelond break-it on a new line means we really need to
;; break it.
(let (closer)
  ;;==============================================
;;
;; output a simple one, and return next x and
  (defun tpanel-layout-simple (ptr x)
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
  (defun tpanel-layout-exp(ptr x)
    ;; (format t "~%tpanel-layout-exp ~A ~A" ptr x)
    (prog ()
       (mvsetq (ptr x) (tpanel-layout-simple ptr x)); we _know_ it is a (
       again
       (let ((kind  (xbuf-kind ptr)))
	 (case kind
	   (#.+KIND-OPEN+   (mvsetq (ptr x)(tpanel-layout-exp ptr x)))
	   (#.+KIND-NORMAL+ (mvsetq (ptr x)(tpanel-layout-simple ptr x)))
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
  (defun tpanel-layout-try-simple (tpanel ptr x)
    (in-tpanel (tpanel)
      (mvbind (proposed-ptr proposed-x) (tpanel-layout-simple ptr x)
	(when (< proposed-x mr.)
	  (values proposed-ptr proposed-x )))))
  
  (defun tpanel-layout-try-exp (tpanel ptr x)
    (in-tpanel (tpanel)
      (mvbind (proposed-ptr proposed-x) (tpanel-layout-exp ptr x)
	(when (< proposed-x mr.)
	  (values proposed-ptr proposed-x )))))
  
  
  
  (defun tpanel-layout (tpanel ptr x indent)
    ;; (format t "~%---~A" indent)
    (in-tpanel (tpanel)
      (labels ((cr ()
		 (loop while closer do
		      (mvsetq (ptr x)(tpanel-layout-simple (pop closer) x )))
		 (terpri)
		 (setf x indent)
		 ))
	;; process opener
	(mvsetq (ptr x) (tpanel-layout-simple ptr x))
	(prog ((p ptr))
	   ;;-------------------------------------------
	 again
	 (when (plusp (xbuf-data-length p))
	   (case (xbuf-kind p)
	     (#.+KIND-NORMAL+ (mvbind (pp px) (tpanel-layout-try-simple tpanel p x)
				(if pp (setf p pp  x px)
				    ;; either :wrap-it or :break-it requires a new line.
				    (progn ; if it does not fit, go to next line
				      (terpri)    ;; TODO: increment y
				      (setf x indent) ))))
	     (#.+KIND-OPEN+
	      (format t "|IN|")
	      (let ((closer1 closer))
		(mvbind (pp px) (tpanel-layout-try-exp tpanel p x)
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
			(mvsetq (p x) (tpanel-layout tpanel p x x))
			;;		    (format t "|PART ~A|" pp )
			)))))
	     (#.+KIND-CLOSE+
	      (push p closer)
	      (format t "|CLOSER|")
	      (return-from tpanel-layout (values p x))))
	   (go again))
	 
	 )))))


(defun tpanel-layout-in (tpanel)
  (in-tpanel (tpanel)
    (setf (xbuf-flags ptr.) 0) ;;terminate
    (tpanel-layout tpanel (inc-pointer buf. +xbuf-prefix+) 0 0 )))


(defun ttt ()
  (setf *tpanel* (make-tpanel))
  (tpanel-append *tpanel* '(defmethod tpanel-append (tpanel (obj list) cons)
			    (tpanel-maybe-space tpanel)
			    (tpanel-append-prim
			     tpanel obj "("
			     0 0 (pen-pic *pen-white*) (pen-pic *pen-black*) )
			    (tpanel-clear-halfspace tpanel)
			    (loop for cons on obj do
				 (tpanel-append tpanel (car cons) cons))
			    (tpanel-append-prim
			     tpanel obj ")"
			     0 0 (pen-pic *pen-white*) (pen-pic *pen-black*) )
			    (tpanel-set-halfspace tpanel)) nil))
