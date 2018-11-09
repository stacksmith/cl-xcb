(in-package :xcb)
;; version 2: 6-bytes overhead
;;  Chunked screen support.  Layout not good.
;;
;; We use a single foreign buffer for all XCB xrender string data (xbufs).
;; Each chunk is written into line's chunk array along with style info and
;; pixel width and asc/desc info.
;;
;; During layout we figure out the x/y pixel positions
;;==============================================================================
;; XCB xbuf accessors for xrender string-compositing functions.  Note
(defmacro xbuf-pix-width (xbuf)
  `(mem-ref ,xbuf :UINT16 -4))
(defmacro xbuf-styndex (xbuf)
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
(defconstant  +xbuf-prefix+ 4)

;;------------------------------------------------------------------------------
;; xbuf-next
;;            Advance xbuf from its normal (+ n +xbuf-prefix) pos to next.
(defmacro xbuf-next (xbuf)
  `(let ((xbuf ,xbuf))
     (declare (type foreign-pointer xbuf))
     (inc-pointer xbuf (+ +xbuf-prefix+ (xbuf-byte-length xbuf)) )))
(defun xbuf-print (xbuf &optional (s t))
  (loop for i from 0 below (xbuf-data-length xbuf) do
       (princ (code-char (xbuf-ref xbuf i)) s)))
;;------------------------------------------------------------------------------
;; xbuf-set
;;            Set xbuf to styled text of the indexed object

(defun xbuf-set (xbuf string styndex)
  (format t "~%~X:~A" xbuf string)
  (setf (xbuf-styndex xbuf) styndex)
  (let ((pix-width 0)
	(font (style-font (aref *styles* styndex))))
    (loop for i from 0 below (length string)
       for offset from 8 by 4
       for code = (char-code (char string i))
       do
	 (incf pix-width (glyph-assure font code))
       ;;       (format t "~%CODE ~A ~A" (code-char code) code)
	 (setf (mem-ref xbuf :UINT32 offset) code)
	 
       finally; update pixel width and glyph count
	 (setf (xbuf-pix-width xbuf) pix-width; store pix-width
	       (xbuf-data-length xbuf) i)
       ;; bump pointer to next position
	 (return (inc-pointer xbuf (+ offset 4 +xbuf-prefix+))))))

;;------------------------------------------------------------------------------
;; Space
#||
(defparameter *the-cond-space* (defstruct cond-space))
(defmethod display-as ((obj cond-space) screen)
  (screen-append-prim screen obj " " +KIND-NORMAL+ style-space))
||#


;;==============================================================================
;;
;; SECTION - a bunch of styled text ready for layout
;;
(defstruct (section (:constructor make-section%))
  (buf nil :type foreign-pointer)
  (ptr nil :type foreign-pointer) ;; write pointer

  (indent 0  :type U32)
  (lines nil :type simple-vector)
  (lindex 0  :type U32)
  )
(defmacro in-section ((name section) &body body)
  `(let ((,name ,section))
     (with-accessors ((buf. section-buf)
		      (ptr. section-ptr)
		      (lines. section-lines)
		      (lindex. section-lindex)) ,name
       ,@body)))

(defun make-section (&key (buffer-size #x10000))
  (let ((buf (foreign-alloc :char :count buffer-size)))
  ;;  (setf (aref arr 0) *the-cond-space*)
    (make-section% :buf buf
		   :ptr (inc-pointer buf +xbuf-prefix+)
		   :lines (make-array 128 :element-type t ))))

(defmethod print-object ((o section) stream)
  (print-unreadable-object (o stream :type t )
    (in-section (section o)
      (loop for p = (inc-pointer buf. +xbuf-prefix+) then (xbuf-next p)
	 until (pointer-eq p ptr.)
	 do
	   (xbuf-print p stream)
	   (princ #\   stream)))))



(defun section-append (section string  styndex)
  (in-section (section section)
    (setf ptr. (xbuf-set ptr. string styndex))))

(defun section-clear (section)
  (in-section (section section)
    (setf ptr. (inc-pointer buf. +xbuf-prefix+))))

(defun section-first-xbuf (section)
  (inc-pointer (section-buf section) +xbuf-prefix+))

;;====================================
;; layout
(defstruct (line (:include pt2))
  (ptr nil :type foreign-pointer)
  )
#||
(defun layout-section (section pt2)
  (prog ((x (pt2-x1 pt2))
	 (y (pt2-y1 pt2))
	 (p (section-first-xbuf section))
	 line)
   start-line
   (setf line (make-line :x1 x :y1 y :x2 x :y2 y :ptr p))
   append-line

   (let ((prop-x (+ x (xbuf-pix-width p))))
     (if (< prop-x (pt2-x2 pt2))
	 (progn
	   (setf x prop-x)
	   (setf p (xbuf-next p))
	   (when (pointer-eq p (section-ptr section))
	     
	     (go done))
	   (go append-line))
	 (go start-line)))
   done
   

   ))
||#
