(in-package :xcb)

;;==============================================================================
;;
;; SCREEN
;;
(defstruct (screen (:constructor make-screen%))
  (buf nil :type foreign-pointer)
  (ptr nil :type foreign-pointer) ;; write pointer

  (chunks nil :type simple-vector)
  (chindex 0  :type U32)
  
  (halfspace 0 :type U32)

  (indent 0  :type U32)
  (lines nil :type simple-vector)
  (lindex 0  :type U32)

  (margin-right 400 :type U32)
  )
(defmacro in-screen ((screen) &body body)
  `(let ((screen ,screen))
     (with-accessors ((buf. screen-buf)
		      (ptr. screen-ptr)
		      (chunks. screen-chunks)
		      (chindex. screen-chindex)
		      (halfspace. screen-halfspace)
		      (indent. screen-indent)
		      (lines. screen-lines)
		      (lindex. screen-lindex)
		      (margin-r. screen-margin-right)
		      ) screen
       ,@body)))

(defun make-screen (&key (buffer-size #x10000) (array-size 1024) )
  (let ((buf (foreign-alloc :char :count buffer-size))
	(arr (make-array array-size :element-type t)))
  ;;  (setf (aref arr 0) *the-cond-space*)
    (make-screen% :buf buf
		  :ptr (inc-pointer buf +xbuf-prefix+)
		  :chunks arr
		  :chindex 0
		  :lines (make-array 128 :element-type t ))))

(defmethod print-object ((o screen) s)
  (print-unreadable-object (o s :type t )
    (loop for i below (screen-chindex o)
       do (princ (aref (screen-chunks o) i) s))))

(defun screen-append-prim (screen obj string flags styndex)
  (with-accessors ((chunks. screen-chunks) (index. screen-chindex)
		   (ptr. screen-ptr)) screen
    (let ((index index.))
      (setf (aref chunks. index) obj) ;; store object in our lisp array;
      ;; write xbuf, and update ptr
      (setf ptr. (xbuf-set ptr. index string flags styndex))
      ;; store 0-length at ptr to indicate final object- for now...TODO:
      (setf (xbuf-data-length ptr.) 0)
      (incf index.)
      obj)))

(defparameter *screen* (make-screen)
  )

(defstruct displayable )
(defmethod display-as ((obj displayable) screen)
  )


(defstruct (disp-space (:include displayable)))
(defmethod print-object ((o disp-space) s)
  (print-unreadable-object (o s  )
    (princ " " s)
))

(defparameter *the-space* (make-disp-space))
(defmethod screen-append (screen (obj displayable) cons)
  (screen-append-prim screen *the-space* " " +KIND-NORMAL+ style-space)
  )


;; -----------------------------------------------------------------------------
;; half-space
(defmacro screen-incf-halfspace (screen)
  `(incf (screen-halfspace ,screen)))
(defmacro screen-clear-halfspace (screen)
  `(setf (screen-halfspace ,screen) 0))
(defmacro screen-set-halfspace (screen)
  `(setf (screen-halfspace ,screen) 1))

(defun screen-maybe-space (screen)
  (when (> (screen-incf-halfspace screen) 1)
    (screen-append screen *the-space* nil)
    (screen-clear-halfspace screen)))
;;====================================================================
;; Append an object to display list of screen.
;;
;; To facilitate editing of structures, we store the cons if possible.
;; If not, the object is not editable or updatable.
(defmethod screen-append (screen (obj symbol) cons)
  (screen-maybe-space screen)
  (screen-append-prim screen cons (string-downcase (symbol-name obj))
		      +KIND-NORMAL+ style-symbol)
  (screen-set-halfspace screen))

(defmethod screen-append (screen (obj string) cons)
  (screen-maybe-space screen)
  (screen-append-prim screen obj "\""  +KIND-NORMAL+ style-string)
  (screen-append-prim screen cons obj  +KIND-NORMAL+ style-string)
  (screen-append-prim screen obj "\""  +KIND-NORMAL+ style-string)
  (screen-set-halfspace screen))

(defmethod screen-append (screen (obj number) cons)
  (screen-maybe-space screen)
  (screen-append-prim screen cons (format nil "~A" obj)  +KIND-NORMAL+ style-literal)
  (screen-set-halfspace screen))

(defmethod screen-append (screen (obj list) cons)
  (screen-maybe-space screen)
  (screen-append-prim  screen obj "(" +KIND-OPEN+ style-paren)
  (screen-clear-halfspace screen)
   (loop for cons on obj do
	(screen-append screen (car cons) cons))
   (screen-append-prim  screen obj ")" +KIND-CLOSE+ style-paren)
  (screen-set-halfspace screen))

;; Layout the screen into lines.
;; Note: break-it comes in two stages: first break-it triggers a newline
;; and another check; second break-it on a new line means we really need to
;; break it.
(let (closer)
  ;;==============================================
;;
;; output a simple one, and return next x and
  (defun screen-layout-simple (ptr x)
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
  (defun screen-layout-exp(ptr x)
    ;; (format t "~%screen-layout-exp ~A ~A" ptr x)
    (prog ()
       (mvsetq (ptr x) (screen-layout-simple ptr x)); we _know_ it is a (
       again
       (let ((kind  (xbuf-kind ptr)))
	 (case kind
	   (#.+KIND-OPEN+   (mvsetq (ptr x)(screen-layout-exp ptr x)))
	   (#.+KIND-NORMAL+ (mvsetq (ptr x)(screen-layout-simple ptr x)))
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
  (defun screen-layout-try-simple (screen ptr x)
    (in-screen (screen)
      (mvbind (proposed-ptr proposed-x) (screen-layout-simple ptr x)
	(when (< proposed-x margin-r.)
	  (values proposed-ptr proposed-x )))))
  
  (defun screen-layout-try-exp (screen ptr x)
    (in-screen (screen)
      (mvbind (proposed-ptr proposed-x) (screen-layout-exp ptr x)
	(when (< proposed-x margin-r.)
	  (values proposed-ptr proposed-x )))))
  
  
  
  (defun screen-layout (screen ptr x indent)
    ;; (format t "~%---~A" indent)
    (in-screen (screen)
      (labels ((cr ()
		 (loop while closer do
		      (mvsetq (ptr x)(screen-layout-simple (pop closer) x )))
		 (terpri)
		 (setf x indent)
		 ))
	;; process opener
	(mvsetq (ptr x) (screen-layout-simple ptr x))
	(prog ((p ptr))
	   ;;-------------------------------------------
	 again
	 (when (plusp (xbuf-data-length p))
	   (case (xbuf-kind p)
	     (#.+KIND-NORMAL+ (mvbind (pp px) (screen-layout-try-simple screen p x)
				(if pp (setf p pp  x px)
				    ;; either :wrap-it or :break-it requires a new line.
				    (progn ; if it does not fit, go to next line
				      (terpri)    ;; TODO: increment y
				      (setf x indent) ))))
	     (#.+KIND-OPEN+
	      (format t "|IN|")
	      (let ((closer1 closer))
		(mvbind (pp px) (screen-layout-try-exp screen p x)
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
			(mvsetq (p x) (screen-layout screen p x x))
			;;		    (format t "|PART ~A|" pp )
			)))))
	     (#.+KIND-CLOSE+
	      (push p closer)
	      (format t "|CLOSER|")
	      (return-from screen-layout (values p x))))
	   (go again))
	 
	 )))))


(defun screen-layout-in (screen)
  (in-screen (screen)
    (setf (xbuf-flags ptr.) 0) ;;terminate
    (screen-layout screen (inc-pointer buf. +xbuf-prefix+) 0 0 )))


(defun ttt ()
  (setf *screen* (make-screen))
  (screen-append *screen* '(defmethod screen-append (screen (obj list) cons)
			    (screen-maybe-space screen)
			    (screen-append-prim
			     screen obj "("
			     0 0 (pen-pic *pen-white*) (pen-pic *pen-black*) )
			    (screen-clear-halfspace screen)
			    (loop for cons on obj do
				 (screen-append screen (car cons) cons))
			    (screen-append-prim
			     screen obj ")"
			     0 0 (pen-pic *pen-white*) (pen-pic *pen-black*) )
			    (screen-set-halfspace screen)) nil))
