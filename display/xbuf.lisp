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
(defmacro xbuf-flags (xbuf)
  `(mem-ref ,xbuf :UINT8 0))
(defmacro xbuf-styndex (xbuf)
  `(mem-ref ,xbuf :UINT8 1))
(defmacro xbuf-pix-width (xbuf)
  `(mem-ref ,xbuf :UINT16 2))
(defmacro xbuf-chindex (xbuf)
  `(mem-ref ,xbuf :UINT16 4))

;; 16 - XCB-native
(defmacro xbuf-data-length (xbuf)
  `(mem-ref ,xbuf :UINT32 6))
(defmacro xbuf-x (xbuf)
  `(mem-ref ,xbuf :UINT16 10))
(defmacro xbuf-y (xbuf)
  `(mem-ref ,xbuf :UINT16 12))
(defmacro xbuf-ref (xbuf index)
  `(mem-ref ,xbuf :UINT32 (+ 14 (* 4 ,index))))
(defmacro xbuf-byte-length (xbuf)
  `(+ 14 (* 4 (xbuf-data-length ,xbuf))))
(defconstant  +xbuf-prefix+ 0)
(defconstant  +xbuf-data+ 14)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +KIND-END+ 0)
  (defconstant +KIND-OPEN+ 1)
  (defconstant +KIND-CLOSE+ 3)
  (defconstant +KIND-NORMAL+ 2))
(defmacro xbuf-kind (xbuf)
  `(logand 3 (xbuf-flags ,xbuf)))

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
       for offset from +XBUF-DATA+ by 4
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
(defun xbuf-dump (xbuf)
  (format t "~%<ch~A|~A|~A|#~A|(~A,~A)~A:"
	  (xbuf-flags xbuf)
	  (xbuf-styndex xbuf)
	  (xbuf-pix-width xbuf)
	  (xbuf-chindex xbuf)
	  (xbuf-x xbuf)
	  (xbuf-y xbuf)
	  (xbuf-data-length xbuf))
  (let ((len (xbuf-data-length xbuf)))
    (loop for i below len do
	 (princ (code-char (xbuf-ref xbuf i)))))
  (format t ">")
  (xbuf-next xbuf)
  )
;;------------------------------------------------------------------------------
