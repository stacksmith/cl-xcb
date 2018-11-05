(in-package :xcb)

;;============================================================================
;; A Pen is an xrender picture 1x1 along with a 64-bit #xAAAABBBBGGGGRRRR.
(defstruct (pen (:constructor make-pen%))
  abgr64 pic)


(defun make-pen (abgr64)
;;  (format *q* "attributes:createing pen ~A~&" abgr64)
  (with-ids (pixmap picture)
    (check (create-pixmap c 32 pixmap root-window 1 1))
    (w-foreign-values (vals :uint32 REPEAT-NORMAL)
      ;; picture
      (check (create-picture c picture pixmap +ARGB32+  CP-REPEAT vals)))
	 ;; fill picture with color (using repeat)
    (w-foreign-values (rect :int16 0 :int16 0 :uint16 1 :uint16 1)
      (check (fill-rectangles c OP-SRC picture abgr64 1 rect)))
    (check (free-pixmap c pixmap));; since it never changes?
    (make-pen% :abgr64 abgr64 :pic picture)))


;;------------------------------------------------------------------------------
;; pen-from-lemcolor (which may be "Red" or "#FF0012" or nil.
;; nil pens mean use current default
#||(defun pen-from-lemcolor (lemcolor)
  (when lemcolor
    (pen-create (rgblist->64 (lemcolor->rgblist lemcolor)))))
;;------------------------------------------------------------------------------
(defun lemcolor->rgblist (lemcolor)
  (check-type lemcolor string)
  (if (char= #\# (char lemcolor 0))
      (list (parse-integer lemcolor :radix 16 :start 5 :end 7)
	    (parse-integer lemcolor :radix 16 :start 3 :end 5)
	    (parse-integer lemcolor :radix 16 :start 1 :end 3) )
      (lem:get-rgb-from-color-name lemcolor)))
||#
;;------------------------------------------------------------------------------
(defun rgblist->64 (rgblist)
   (logior #xFFFF000000000000
	     (ash (the (unsigned-byte 8) (first rgblist))   8)
	     (ash (the (unsigned-byte 8) (second rgblist)) 24)
	     (ash (the (unsigned-byte 8) (third rgblist))  40)))

(defparameter *pen-black* nil)
(defparameter *pen-white* nil)
(defparameter *pen-trans* nil)

(defun init-pens ()
  (setf *pen-black* (make-pen #xFFFF000000000000)
	*pen-white* (make-pen #xFFFFFFFFFFFFFFFF)
	*pen-trans* (make-pen #x0000000000000000)))

(defstruct style
  (font nil :type font)
  (fore nil :type pen)
  (back nil :type pen))

(defparameter *styles* (make-array 256 :initial-element nil))
(defparameter *styles-init*
  `((,*font-normal*  0 0)                 ;0 transparent space
    (,*font-normal*  #xFFFFFFFFFFFFFFFF 0);1 white
    (,*font-normal*  #xFFFF6262C1C12C2C 0);2 emerald text
    (,*font-normal*  #xFFFFFFFF00000000 0);3 blue - literal
    (,*font-normal*  #xFFFF0F004300BD00 0);4 tia maria - string
   
    )
  )

(defconstant style-space 0)
(defconstant style-paren 1)
(defconstant style-symbol 2)
(defconstant style-literal 3)
(defconstant style-string 4)



(defun init-styles ()
  (setf *styles*
	   (make-array
	    (length *styles-init*)
	    :initial-contents
	    (loop for ilist in *styles-init*
	       collect (make-style
			:font (car ilist)
			:fore (make-pen (cadr ilist))
			:back (make-pen (caddr ilist)))))))


;;==============================================================================
;; Attributes
;;
;; Stored in %internal-value: fore and background pens.
;;


;;------------------------------------------------------------------------------
;; attribute-check: Attributes coming from lem may be new; in such a case,
;; create pens and attach attr as %internal
;; 
#||

(defun attribute-check (attribute-or-name)
  (let ((attribute (lem::ensure-attribute attribute-or-name nil)))
    (when attribute
      (unless (lem::attribute-%internal-value attribute)
	(let ((bg-pen (pen-from-lemcolor
		       (lem::attribute-background attribute)))
	      (fg-pen (pen-from-lemcolor
		       (lem::attribute-foreground attribute))))
	  (setf (lem::attribute-%internal-value attribute)
		(cons fg-pen bg-pen))))
#||      (format *q* "ATTRIBUTE-CHECK: ~A ~A ~A ~A ~A~&"
	      (lem::attribute-foreground attribute)
	      (lem::attribute-background attribute)
	      (lem::attribute-reverse-p attribute)
	      (lem::attribute-bold-p attribute)
	      (lem::attribute-underline-p attribute))
||#)

    attribute))
||
;;------------------------------------------------------------------------------
;; attribute-decode
;;
(defun attribute-decode (attribute)
  "values pen-fg pen-bg boldp underlinep"
  (let ((a (attribute-check attribute))) ; make sure attribute is good
    (if a
	(let ((bold (lem:attribute-bold-p a))
	      (underline (lem:attribute-underline-p a)))
	;;  (format *q* "ATTR-DECODE ~A ~A~&" bold underlineg)
	  (destructuring-bind (f . b) (lem::attribute-%internal-value a)
	    (if (lem:attribute-reverse-p a)
		(values (or b (bg *w*))(or f (fg *w*)) bold underline)
		(values (or f (fg *w*))(or b (bg *w*)) bold underline))))
	(values (fg *w*) (bg *w*) nil nil))))
||#
#||
(defstruct attr
  (font *font-normal* :type font )
  (fore (make-pen #xFFFFFFFFFFFFFFFF) :type pen)
  (back (make-pen #xFFFF000000000000) :type pen))

(defparameter *attr-normal* nil)
(defun init-attrs ()
  (setf *attr-normal* (make-attr )))
||#
