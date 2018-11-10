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
;; XCB xbuf accessors for xrender string-compositing functions.  Notes:
;; * styndex, the initial UINT16, 
(defmacro xbuf-back (xbuf)      `(mem-ref ,xbuf :INT32 -4))
(defmacro xbuf-attr (xbuf)      `(mem-ref ,xbuf :UINT16 0))
(defmacro xbuf-pix-width (xbuf) `(mem-ref ,xbuf :UINT16 2))
(defmacro xbuf-owner (xbuf)     `(mem-ref ,xbuf :UINT32 4))
;; XCB-native
(defmacro xbuf-glyph-cnt (xbuf)  `(mem-ref ,xbuf :UINT32  8))
(defmacro xbuf-x (xbuf)          `(mem-ref ,xbuf :UINT16 12))
(defmacro xbuf-y (xbuf)          `(mem-ref ,xbuf :UINT16 14))


(defmacro xbuf8-ref (xbuf index)  `(mem-ref ,xbuf :UINT32 (+ 16 (* 4 ,index))))
(defmacro xbuf32-ref  (xbuf index)  `(mem-ref ,xbuf :UINT32 (+ 16 ,index)))

;;------------------------------------------------------------------------------
;; Attributes are as follows:   READ-ONLY!
;; 0 means end-marker
;; bit 0 is byte-glyph; when 0, glyphs are <256
;; bits 1-15 are style-index
(defmacro attr-ascii   (attr)   `(oddp ,attr))
(defmacro attr-styndex (attr)   `(ash ,attr -1))
(defmacro attr-end     (attr)   `(zerop ,attr))

(defmacro xbuf8-bytes (xbuf)   `(+ 16 (xbuf-glyph-cnt ,xbuf)))
(defmacro xbuf32-bytes (xbuf)  `(+ 16 (* 4 (xbuf-glyph-cnt ,xbuf))))

(defmacro xbuf-prev (xbuf) `(inc-pointer ,xbuf (xbuf-back ,xbuf)))
;;------------------------------------------------------------------------------
;;    
(defun xbuf-print (xbuf &optional (s t))
  (let ((attr (xbuf-attr xbuf)))
    (if (attr-ascii attr)
	(loop for i from 0 below (xbuf-glyph-cnt xbuf) do
	     (princ (code-char (xbuf8-ref xbuf i)) s))
	(loop for i from 0 below (xbuf-glyph-cnt xbuf) do
	     (princ (code-char (xbuf32-ref xbuf i)) s)))))

(defmacro xbuf-valid-p (ptr)
  "Does this xbuf have a length?"
  `(plusp (mem-ref ,ptr :UINT32)))
;;------------------------------------------------------------------------------
;; xbuf-set
;;            Set xbuf to styled text of the indexed object
;; * set glyph count
;;
(defun xbuf32-set (xbuf string styndex owner-index)
  (setf (xbuf-owner xbuf) owner-index
	(xbuf-attr xbuf) (ash styndex 1))
  (loop for i from 0 below (length string)
     for offset from 16 by 4
     for code = (char-code (char string i))
     do
       (setf (mem-ref xbuf :UINT32 offset) code)
     finally; update pixel width and glyph count
       (setf (xbuf-glyph-cnt xbuf) i)
       (let ((retval (+ offset 8)))
	 (setf (mem-ref xbuf :INT32 (+ offset 4)) (- retval))
	 ;; bump pointer to next position
	 (return retval))))

(defun xbuf-set (xbuf string styndex owner-index)
  (setf (xbuf-owner xbuf) owner-index
	(xbuf-attr xbuf) (ash styndex 1))
  (prog ()
     (loop for i from 0 below (length string)
	for offset from 16 
	for code = (char-code (char string i))
	do
	  (when (> code 255)
	    (go big))
	  (setf (mem-ref xbuf :UINT8 offset) code)
	finally; update pixel width and glyph count
	  (setf (xbuf-glyph-cnt xbuf) i)
	  (let ((retval (+ offset 5)))
	    (setf (mem-ref xbuf :INT32 (1+ offset)) (- retval))
	    ;; bump pointer to next position
	    (return-from xbuf-set retval)))
     big
     (xbuf32-set xbuf string styndex owner-index)))
