(in-package :xcb)
;;==============================================================================
;;
;; ft2.lisp  - low level freetype2 interfaces.
;;
;; The freetype2 library is used to extract glyph bitmaps and convert them to
;; XRender ABGR format.
;;
;; The client provides glyph information as well.
;;
;; The client must also track fonts.
;;
;; Note: for conversion, the bitmap is wider than expected to accomodate
;; antialiasing edges...
;;
;; Unicode (21-bit) glyph space is managed as 128-glyph pages in a
;; 3-level trie, with a fastpath for ASCII (7-bit).  The trie is filled on
;; as-needed basis, and contains advance values for the glyphs.
;;
(defparameter gs nil)

;; missing from ft2 due to old age...
(defcfun ("FT_Library_SetLcdFilter" set-lcd-filter&) :uint32
  (library ft2::ft-library)
  (filter :uint32))

(defparameter *xcb-context* nil)

(defun ft2init ()
  (set-lcd-filter& ft2::*library* 1)
  (setf *xcb-context* *conn*))

(defstruct (font(:constructor make-font%))
  face glyphset
  ascender descender underline-position underline-thickness
  glyph-trie ascii )

(defun make-font (&key path w h (hres 85) (yres 88))
  (let ((glyphset (generate-id *conn*))
	(face (ft2:new-face path)))
    (ft2:set-char-size face w h hres yres)
    (check (create-glyph-set *conn* glyphset +ARGB32+ ))
    
    (let* ((metrics (ft2::ft-size-metrics (ft2::ft-face-size face)))
	   (x-scale (ft2::ft-size-metrics-x-scale metrics) )
	   (y-scale (ft2::ft-size-metrics-y-scale metrics))
	   
	   ;; TODO:
	   (font
	    (make-font%
	     :face  face
	     :glyphset glyphset
;;	     :pagemap (make-array #x2000 :element-type 'bit :initial-element 0)
;;	     :toy-advance  (floor (ft2:get-advance face #\w))
	     :ascender (/ (ft2::ft-size-metrics-ascender metrics) 64)
	     :descender (/ (ft2::ft-size-metrics-descender metrics) 64)
	     :underline-position (/ (* y-scale (ft2::ft-face-underline-position face)) (* 64 #x10000))
	     :underline-thickness (/ (* y-scale (ft2::ft-face-underline-thickness face)) (* #x10000 64))
	     :glyph-trie (make-array 128 :initial-element nil )
	     )))
      ;; Pre-load ascii glyphs
      (setf (font-ascii font)(load-glyphpage font 32))
      font)))


#||
(let ()
    (values (ft-26dot6-to-float (ft-size-metrics-x-ppem metrics))
            (ft-26dot6-to-float (ft-size-metrics-y-ppem metrics))
            (ft-26dot6-to-float (ft-size-metrics-x-scale metrics))
            (ft-26dot6-to-float (ft-size-metrics-y-scale metrics))
            (ft-26dot6-to-float (ft-size-metrics-ascender metrics))
            (ft-26dot6-to-float (ft-size-metrics-descender metrics))
            (ft-26dot6-to-float (ft-size-metrics-height metrics))
            (ft-26dot6-to-float (ft-size-metrics-max-advance metrics))))(defmethod initialize-instance :after ((f font) &key path size)
  (with-slots (face glyphset) f
    (setf face (ft2:new-face path))
    (ft2:set-char-size face (* size 64)(* size 64) 85 88)
    
    (setf glyphset )
    (check (create-glyph-set *conn* glyphset +ARGB32+ ))
    ;; low glyphs are always loaded
    (loop for n from 0 to 255 do
	  (load-glyph f n))
      ;;      (load-glyph f gs 992)an
      ;;      (load-glyph f gs 994)
      ;;      (load-glyph f gs 1046)

    (values gs f)))
||#
(defmethod destroy ((f font))
  (with-slots (glyphset face) f
    (check (free-glyph-set *conn* glyphset))
    (setf face nil)))

;;==============================================================================
;; make an x glyph bitmap, and convert rows.  Free later!
(defun make-glyph-bitmap (source w h s-pitch )
   (let* ((d-pitch (* 4 w))
	 (bitmap (foreign-alloc :uint32 :count (* d-pitch h))))
    (flet ((convert-row (s-off d-off)
	     (loop for i from 0 below w
		for s-off from s-off by 3 
		for d-off from d-off by 4 do
		  (let ((srgb (mem-ref source :uint32 s-off)))
		    (setf (mem-ref bitmap :uint32 d-off)
			  (logior (logand srgb #x00FF00)
				  (ldb (byte 8 16) srgb)
				  (ash (ldb (byte 8 0) srgb) 16)))))))
      (unless (zerop w)
	(loop for row from 0 below h
	   for s-off from 0 by s-pitch
	   for d-off from 0 by d-pitch do
	     (convert-row s-off d-off))))
    bitmap))

;;==============================================================================
;; create a foreign glyphinfo-t
;;
(defun make-glyphinfo (w h x y x-adv y-adv)
  (foreign-values
   :uint16 w
   :uint16 h
   :int16  x
   :int16  y
   :int16 x-adv
   :int16  y-adv))
;;==============================================================================
;; dump resultant bitmap for debug only
(defun dump-bitmap (ptr w h)
  (terpri)
  (loop for y from 0 below h do
       (loop for x from 0 below w do
	    (format t "~8x " (ash (mem-ref ptr :uint32) -8))
	    (cffi::incf-pointer ptr 4))
       (terpri)))
;;==============================================================================
;;  ;; #x30024=  0011 0000 0000 0010 1000 = render,lcd autohint
;; TODO: if this is really how we do it, more than one glyph may be sent to X
(defun load-glyph (font code)
  (with-slots (face glyphset) font
    (let* ((ft2-glyph-index (ft2:get-char-index face code))
	   (unused  (ft2:load-glyph face ft2-glyph-index #x30024 ))
	   (glyphslot (ft2::ft-face-glyph face))
	   (bitmap    (ft2::ft-glyphslot-bitmap glyphslot))
	   (left      (ft2::ft-glyphslot-bitmap-left glyphslot))
	   (top       (ft2::ft-glyphslot-bitmap-top  glyphslot))
	   (advance-x  ;;(ft2::get-loaded-advance face nil)
	    (ft2::ft-vector-x (ft2::ft-glyphslot-advance glyphslot)))
	   ;; and
	   (w         (/ (ft2::ft-bitmap-width bitmap) 3))
	   (h         (ft2::ft-bitmap-rows bitmap))
	   (s-pitch        (ft2::ft-bitmap-pitch bitmap))
	   (source         (ft2::ft-bitmap-buffer bitmap))
	   ;;	 (qqq  (format t "~A: ~A, ~A ~A ~A ~A~&" code advance-x w h s-pitch source))
	   (x-bitmap  (make-glyph-bitmap source w h s-pitch))
)
      (let* ((metrics (ft2::ft-glyphslot-metrics glyphslot))
	     ;; OK.  So technically, metrics should give better positioning, no?
	     ;; In practice, very similar.  But see for yourself.
	     (glyphinfo (make-glyphinfo w h (- left)  top (/ advance-x 64) 0)) 
#||	     (glyphinfo (make-glyphinfo
		    w h
		    (- (/ (ft2::ft-glyph-metrics-hori-bearing-x metrics) 64))
		    (/ (ft2::ft-glyph-metrics-hori-bearing-y metrics) 64)
		    (/ advance-x 64) 0))
	     ||#
	     )
;;	(declare (ignore unused))
	;;----------------------
#||	(format t "~%-------------..~A:" (code-char code) )
	(format t "~% as stored (~A,~A) w:~A h:~A advance:~A"
		(- left) top w h (/ advance-x 64))   
	;; some metrics
	
	(format t "~% metrics w ~A  h ~A"
		(/ (ft2::ft-glyph-metrics-width metrics) 64)
		(/ (ft2::ft-glyph-metrics-height metrics) 64))

	  ;;	(format t "~%         (~A,~A) w:~A h:~A ")

	(format t "~% bearing (~A:~A) adv ~A"
		(/ (ft2::ft-glyph-metrics-hori-bearing-x metrics) 64)
		(/ (ft2::ft-glyph-metrics-hori-bearing-y metrics) 64)
		(/ (ft2::ft-glyph-metrics-hori-advance metrics) 64))
||#
	
	#||	(format t "~% vert (~A:~A) ~A"
	(/ (ft2::ft-glyph-metrics-vert-bearing-x metrics) 64)
	(/ (ft2::ft-glyph-metrics-vert-bearing-y metrics) 64)
	(/ (ft2::ft-glyph-metrics-vert-advance metrics) 64)) ||#
	
	;;(format *q* "WWWWW ~A ~A ~A ~&" (code-char code) (/ advance-x 64) (ft2::get-loaded-advance face nil))
	(w-foreign-values (pcode :uint32 code)
 	  ;;	(format t "~%added glyph ~A" pcode)
	  (check (add-glyphs *xcb-context* glyphset  1 pcode  glyphinfo (* 4 w h) x-bitmap)))
	(foreign-free x-bitmap)
	(foreign-free glyphinfo)
	;; mark glyph as loaded
	advance-x))))

;;==============================================================================
;; Glyph-trie.
;;
;; There are 21 bits allocated to glyphs.  We shall divide these into sets of
;; 128 - 7 bits each.  The bottom set is ASCII, and it has a fast path.
;;
;; The rest are stored in a trie as follows: level0 table has 128 slots for
;; level 1 tables; level-1 tables contain maps of 128 positions for glyphs.
;; Each position holds the glyph's advance value for width calculation.
;;
;; Accessing an empty table triggers a load of all 128 glyphs.
;;
;;==============================================================================
;; Load an entire 128-glyph page containing the code
(defun load-glyphpage (font code)
  (let ((glyphpage (make-array 128 :element-type 'U32)))
    (loop for i from 0 to 127
       for index from (logand code #x1FFF80)
       do (setf (aref glyphpage i)
		(ash (load-glyph font index) -6)))
    glyphpage))


(declaim (inline glyph-assure))
(defun glyph-assure (font code)
  (declare (type U32 code))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-accessors ((ascii font-ascii)) font
    (declare (type (simple-array U32) ascii))
    (if (< code 128)
	(aref ascii code)
	(glyph-assure-long font code))))
(defun glyph-assure-long (font code)
  (declare (type U32 code))
;;  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-accessors ((page0 font-glyph-trie) ) font
    (declare (type simple-vector page0))
    
    (let* ((idx0 (ash code -14))
	   (idx1 (ldb (byte 7 7) code))
	   (page1
	    (or (aref page0 idx0)
		(setf (aref page0 idx0)
		      (make-array 128 :initial-element nil)))))
      (declare (type simple-vector page1))
      (let ((page2
	     (or (aref page1 idx1)
		 (setf (aref page1 idx1)
		       (load-glyphpage font code)))))
	(declare (type simple-array page2))
	(aref page2 (logand code #x7F))))))


(defun ttt (font gt z)
  (declare (optimize (speed 3) (safety 0) (debug 0)))

  

;;  (the fixnum (+ 5 (the fixnum (gt-ref font gt z))))


      
  )
