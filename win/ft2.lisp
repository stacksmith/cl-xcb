(in-package :xcb)
;;==============================================================================
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
;; Currently this is done with a 1024-entry array for the low 1024 glyphs, and
;; the rest is in a hashtable.
;;
;; TODO: rebuild the lost trie implementation:
;; - 128-glyph (7-bit) tries;
;; - low trie (ASCII) is on fast path;
;; - rest are in a 3-level system: leaves contain 128 glyphs, 'page-tables'
;;   128 pointers provide an on-demand access system.
(defparameter gs nil)

;; missing from ft2 due to old age...
(defcfun ("FT_Library_SetLcdFilter" set-lcd-filter&) :uint32
  (library ft2::ft-library)
  (filter :uint32))

(defparameter *xcb-context* nil)

(defun ft2init ()
  (set-lcd-filter& ft2::*library* 1)
  (setf *xcb-context* c))

(defstruct (font(:constructor make-font%))
  face glyphset pagemap
  ascender descender underline-position underline-thickness
  toy-advance)

(defun make-font (&key path w h (hres 85) (yres 88))
  (let ((glyphset (generate-id c))
	(face (ft2:new-face path)))
    (ft2:set-char-size face w h hres yres)
    (check (create-glyph-set c glyphset +ARGB32+ ))
    
    (let* ((metrics (ft2::ft-size-metrics (ft2::ft-face-size face)))
	   (x-scale (ft2::ft-size-metrics-x-scale metrics) )
	   (y-scale (ft2::ft-size-metrics-y-scale metrics))
	   ;; TODO:
	   (font
	    (make-font%
	     :face  face
	     :glyphset glyphset
	     :pagemap (make-array #x2000 :element-type 'bit :initial-element 0)
	     :toy-advance  (floor (ft2:get-advance face #\w))
	     :ascender (/ (ft2::ft-size-metrics-ascender metrics) 64)
	     :descender (/ (ft2::ft-size-metrics-descender metrics) 64)
	     :underline-position (/ (* y-scale (ft2::ft-face-underline-position face)) (* 64 #x10000))
	     :underline-thickness (/ (* y-scale (ft2::ft-face-underline-thickness face)) (* #x10000 64))
	     )))
      (load-glyph-page font 32)
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
    (check (create-glyph-set c glyphset +ARGB32+ ))
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
    (check (free-glyph-set c glyphset))
    (setf face nil)))

#||
;; load a glyphset
(defun load-gs (filename size )
    
    (let ((f (ft2:new-face filename)))
    (ft2:set-char-size f (* size 64)(* size 64) 85 88)

    (let* ((gs (generate-id c))
	   (cookie (create-glyph-set-checked c gs +ARGB32+ )))
      (unless (null-pointer-p (request-check c cookie))
	(error "at: create-glyph-set"))
      (loop for n from 32 to 127 do	   (load-glyph f gs n))
      (load-glyph f gs 992)
      (load-glyph f gs 994)
      (load-glyph f gs 1046)
      (values gs f))))
||#
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
	)))

  ;;==============================================================================
  ;; Glyphs are loaded a page at a time, and each page has a bit in pagetable
  ;; to indicate it's loaded.
)
(defun load-glyph-page (font code)
  (with-slots (pagemap) font
    (setf (bit pagemap (ash code -8)) 1)
    (loop for n from (logand code #x1FFF00) to (+ code 255) do
	 (load-glyph font n))))
;;------------------------------------------------------------------------------
;; check the code to make sure it's loaded via the pagetable.
(defun glyph-assure-long (font code)
  (declare (type fixnum code)
	   (type font font))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (pagemap) font
    (declare (type simple-bit-vector pagemap))
    (if (zerop (bit pagemap (ash code -8)))
	(load-glyph-page font code))))
;;-----------------------------------------------------------------------------
;; GLYPH-ASSURE - a quick inline check for page0, which is always loaded.
;; otherwise, we check pagetable via function call.
(declaim (notinline glyph-assure))

(defun glyph-assure (font code)
  (declare (type fixnum code)
	   (type font font))
  (declare (optimize (speed 3) (safety 0) (debug 0)))

  (unless (< code 256)
    (glyph-assure-long font code))
  
  (font-toy-advance font))





(defun ttt (font z)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type (unsigned-byte 32) z))

  

    
  (glyph-assure font z)
  (+ z 3)
    
  )
