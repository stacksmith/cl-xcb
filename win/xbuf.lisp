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
