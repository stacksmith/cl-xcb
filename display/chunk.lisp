(in-package :xcb)
;; version 2: 6-bytes overhead
;;  Chunked screen support.
;;
;; We use a single foreign buffer for all XCB xrender string data (xbufs).
;; Each chunk is written into line's chunk array along with style info and
;; pixel width and asc/desc info.
;;
;; During layout we figure out the x/y pixel positions
;;==============================================================================
;; XCB xbuf accessors for xrender string-compositing functions.  Note
(defmacro xbuf-pix-width (xbuf)
  `(mem-ref ,xbuf :UINT16 -6))
(defmacro xbuf-styndex (xbuf)
  `(mem-ref ,xbuf :UINT8 -4))
(defmacro xbuf-flags (xbuf)
  `(mem-ref ,xbuf :UINT8 -3))
(defmacro xbuf-chindex (xbuf)
  `(mem-ref ,xbuf :UINT16 -2))
;; 16 - XCB-native
(defmacro xbuf-data-length (xbuf)
  `(mem-ref ,xbuf :UINT32) )
(defmacro xbuf-x (xbuf)
  `(mem-ref ,xbuf :UINT16 4))
(defmacro xbuf-y (xbuf)
  `(mem-ref ,xbuf :UINT16 6))
(defmacro xbuf-ref (xbuf index)
  `(mem-ref ,xbuf :UINT32 (+ 8 (* 4 ,index))))
(defmacro xbuf-byte-length (xbuf)
  `(+ 8 (* 4 (xbuf-data-length ,xbuf))))
(defconstant  +xbuf-prefix+ 6)

;;------------------------------------------------------------------------------
;; xbuf-next
;;            Advance xbuf from its normal (+ n +xbuf-prefix) pos to next.
(defmacro xbuf-next (xbuf)
  `(let ((xbuf ,xbuf))
     (declare (type foreign-pointer xbuf))
     (inc-pointer xbuf (+ +xbuf-prefix+ (xbuf-byte-length xbuf)) )))
;;------------------------------------------------------------------------------
;; xbuf-set
;;            Set xbuf to styled text of the indexed object

(defun xbuf-set (xbuf index string flags styndex)
  (format t "~%~X:~A" xbuf string)
  (setf (xbuf-styndex xbuf) styndex
	(xbuf-chindex xbuf) index
	(xbuf-flags xbuf) flags)
  (let ((pix-width 0)
	(font (style-font (svref *styles* styndex))))
    (loop for i from 0 below (length string)
       for offset from 8 by 4
       for code = (char-code (char string i))
       do
	 (incf pix-width (glyph-assure font code))
       ;;       (format t "~%CODE ~A ~A" (code-char code) code)
	 (setf (mem-ref xbuf :UINT32 offset) code)
	 
       finally; update pixel width and glyph count
	 (format t "~% flags ~A" (xbuf-flags xbuf))
	 (setf (xbuf-pix-width xbuf) pix-width; store pix-width
	       (xbuf-data-length xbuf) i)
       ;; bump pointer to next position
	 (return (inc-pointer xbuf (+ offset 4 +xbuf-prefix+))))))

;;------------------------------------------------------------------------------
;; Space

(defparameter *the-cond-space* (defstruct cond-space))
(defmethod display-as ((obj cond-space) screen)
  (screen-append-prim screen obj " " 0 style-space))



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
  (screen-append-prim screen *the-space* " " 0 style-space)
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
		      0 style-symbol)
  (screen-set-halfspace screen))

(defmethod screen-append (screen (obj string) cons)
  (screen-maybe-space screen)
  (screen-append-prim screen obj "\""  1 style-string)
  (screen-append-prim screen cons obj  0 style-string)
  (screen-append-prim screen obj "\""  2 style-string)
  (screen-set-halfspace screen))

(defmethod screen-append (screen (obj number) cons)
  (screen-maybe-space screen)
  (screen-append-prim screen cons (format nil "~A" obj)  0 style-literal)
  (screen-set-halfspace screen))

(defmethod screen-append (screen (obj list) cons)
  (screen-maybe-space screen)
  (screen-append-prim  screen obj "(" 1 style-paren)
  (screen-clear-halfspace screen)
   (loop for cons on obj do
	(screen-append screen (car cons) cons))
   (screen-append-prim  screen obj ")" 2 style-paren)
  (screen-set-halfspace screen))

;;==============================================
;;
;; output a simple one, and return next x and
(defun screen-layout-simple (ptr x)
  (let ((len (xbuf-data-length ptr)))
    (loop for i below len do
	 (princ (code-char (xbuf-ref ptr i))) ;;TODO: this is for test output
	 (setf (xbuf-x ptr) x))
    (values (xbuf-next ptr) (+ x (xbuf-pix-width ptr)))))

;; Layout an expression in this line...unconditionally,
;; returning x and next-ptr.
(defun screen-layout-exp(ptr x)
 ;; (format t "~%screen-layout-exp ~A ~A" ptr x)
  (prog ()
   again
   (let ((flg  (logand 3 (xbuf-flags ptr))))
     (mvsetq (ptr x) (screen-layout-simple ptr x))
     (unless (= flg 2)
       (go again))
     (return (values  (xbuf-next ptr) x )))))


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


;; Layout the screen into lines.
;; Note: break-it comes in two stages: first break-it triggers a newline
;; and another check; second break-it on a new line means we really need to
;; break it.
(defun screen-layout (screen ptr x indent)
  (format t "~%---~A" indent)
  (in-screen (screen)
    ;; process opener
    (mvsetq (ptr x) (screen-layout-simple ptr x))
    (prog ((p ptr))
       ;;-------------------------------------------
     again
     (when (plusp (xbuf-data-length p))
       (case (logand 3 (xbuf-flags p))
	 (0 (mvbind (pp px) (screen-layout-try-simple screen p x)
	      (if pp (setf p pp  x px)
		  ;; either :wrap-it or :break-it requires a new line.
		  (progn ; if it does not fit, go to next line
		    (terpri)    ;; TODO: increment y
		    (setf x indent) ))))
	 (1 (format t "|IN|")
	  (mvbind (pp px) (screen-layout-try-exp screen p x)
	      (if pp (setf p pp  x px)
		  ;; either :wrap-it or :break-it requires a new line.
		  (progn ; if it does not fit, go to next line
		    (terpri)    ;; TODO: increment y
		    (setf x indent) ; indent as required
		    (mvsetq (p x) (screen-layout screen p x x))))
	      (format t "|OUT ~A|" pp )))
	 (2 (mvsetq (p x)(screen-layout-simple p x))
	    (return-from screen-layout (values p x))))
       (go again))
       
     )))


(defun screen-layout-in (screen)
  (in-screen (screen)
    (setf (xbuf-data-length ptr.) 0) ;;terminate
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
