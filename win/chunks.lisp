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
(defconstant  +xbuf-prefix+ 8)
(defmacro xbuf-owner (xbuf)     `(mem-ref ,xbuf :UINT32 -8))
(defmacro xbuf-pix-width (xbuf) `(mem-ref ,xbuf :UINT16 -4))
(defmacro xbuf-styndex (xbuf)   `(mem-ref ,xbuf :UINT16 -2))
;; XCB-native
(defmacro xbuf-glyph-cnt (xbuf)  `(mem-ref ,xbuf :UINT32  0))
(defmacro xbuf-x (xbuf)          `(mem-ref ,xbuf :UINT16  4))
(defmacro xbuf-y (xbuf)          `(mem-ref ,xbuf :UINT16  6))
(defmacro xbuf-ref (xbuf index)  `(mem-ref ,xbuf :UINT32 (+ 8 (* 4 ,index))))
(defmacro xbuf-bytes (xbuf)      `(+ 8 (* 4 (xbuf-glyph-cnt ,xbuf))))
(defmacro xbuf-total-bytes (xbuf) `(+ +xbuf-prefix+ (xbuf-bytes ,xbuf)))


;;------------------------------------------------------------------------------
;; xbuf-next - or nil!
;;    
(defmacro xbuf-next (xbuf)
  `(let* ((xbuf ,xbuf)
	  (cnt  (xbuf-glyph-cnt xbuf))) ;; 0 means no!
     (declare (type foreign-pointer xbuf)
	      (type U32 cnt))
     (when (plusp cnt)
       (inc-pointer xbuf (+ +xbuf-prefix+ 8 (* 4 cnt))))))

(defun xbuf-print (xbuf &optional (s t))
  (loop for i from 0 below (xbuf-glyph-cnt xbuf) do
       (princ (code-char (xbuf-ref xbuf i)) s)))

(defmacro xbuf-valid-p (ptr)
  "Does this xbuf have a length?"
  `(plusp (mem-ref ,ptr :UINT32)))
;;------------------------------------------------------------------------------
;; xbuf-set
;;            Set xbuf to styled text of the indexed object
;; * set glyph count
;; * set pixel-width
(defun xbuf-set (xbuf string styndex owner-index)
  (setf (xbuf-owner xbuf) owner-index
	(xbuf-styndex xbuf) styndex)
    

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
	       (xbuf-glyph-cnt xbuf) i)
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
;; terminated by 0-length xbuf during layout.
;;
(defstruct (section (:constructor make-section%))
  (buf nil :type foreign-pointer)  ;; foreign buffer with XCB compositing data
  (ptr nil :type foreign-pointer)  ;; write pointer
  (owners nil :type simple-vector) ;; lot of owner objects

  (lines nil :type simple-vector)
  (lindex 0  :type U32)
  )
;;------------------------------------------------------------------------------
(defmacro in-section ((name section) &body body)
  `(let ((,name ,section))
     (with-accessors ((buf. section-buf)
		      (ptr. section-ptr)
		      (owners. section-owners)
		      (lines. section-lines)
		      (lindex. section-lindex)) ,name
       ,@body)))
;;------------------------------------------------------------------------------
(defun make-section (&key (buffer-size #x10000))
  (let ((buf (foreign-alloc :char :count buffer-size)))
  ;;  (setf (aref arr 0) *the-cond-space*)
    (make-section% :buf buf
		   :ptr (inc-pointer buf +xbuf-prefix+)
		   :owners (make-lot)
		   :lines (make-array 128 :element-type t ))))
;;------------------------------------------------------------------------------
(defun delete-section (section)
  (in-section (section section)
    (foreign-free buf.)))
;;------------------------------------------------------------------------------
(defmethod print-object ((o section) stream)
  (print-unreadable-object (o stream :type t )
    (in-section (section o)
      (loop for p = (section-first-xbuf o) then (xbuf-next p)
	 while p do
	   (xbuf-print p stream)
	   (princ #\   stream)))))
;;------------------------------------------------------------------------------
(defun section-append (section string styndex owner)
  (in-section (section section)
    (let ((owner-index (lot-store owners. owner)))
      (setf ptr. (xbuf-set ptr. string styndex owner-index)))))
;;------------------------------------------------------------------------------
(defun section-first-xbuf (section)
  (inc-pointer (section-buf section) +xbuf-prefix+))
;;------------------------------------------------------------------------------
(defun section-clear (section)
  (in-section (section section)
    (setf ptr. (section-first-xbuf section))
    (lot-clear owners.)))
;;------------------------------------------------------------------------------
;; close the section.
(defun section-terminate (section)
  (setf (mem-ref (section-ptr section) :UINT32) 0))

;;==============================================================================
;; A line structure, with geometry.
(defstruct (line (:include geo))
  (ptr nil :type foreign-pointer))
;;------------------------------------------------------------------------------
;; Strategy: set-up a line with x1/y1 at start pos and
;; x2/y2 at container geometry limits.  This provides for margins.
;;
(defun line-init (line xbuf x1 y1 x2 y2)
  (in-geo (line line)
    (setf x1. x1  ; indented start of line
	  y1. y1  ; upper-left corner
	  x2. x2  ; right margin position
	  y2. y2
	  (line-ptr line) xbuf)
    line))
;;------------------------------------------------------------------------------
;; * add xbufs to line while fit, updating X positions;
;; * keep track of max Y ascender and descender;
;; * calculate baseline based on max ascender;
;; * fixup constituent xbuf Y position to baseline;
;; * return values ptr and next line Y (using max descender)
;;
;; Note: xbuf
(defun line-fill (line)
  (prog ((x (line-x1 line)); running x position
	 (xbuf (line-ptr line)) ;running chunk
	 (right-margin (line-x2 line)))
   again
   (when xbuf
     (let ((proposed-x (+ x (xbuf-pix-width xbuf))))
       (when (< proposed-x right-margin)
	 (setf x proposed-x
	       xbuf (xbuf-next xbuf))
	 (go again))))
   ;; done filling.
   )

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
