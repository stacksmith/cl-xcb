(in-package :xcb)
;;============================================================================
;; A Pen is an xrender picture 1x1 along with a 64-bit #xAAAABBBBGGGGRRRR.
(defstruct (pen (:constructor make-pen%))
  abgr64 pic)

(defun make-pen (abgr64)
;;  (format *q* "attributes:createing pen ~A~&" abgr64)
  (with-ids (pixmap picture)
    (check (create-pixmap *conn* 32 pixmap root-window 1 1))
    (w-foreign-values (vals :uint32 REPEAT-NORMAL)
      ;; picture
      (check (create-picture *conn* picture pixmap +ARGB32+  CP-REPEAT vals)))
	 ;; fill picture with color (using repeat)
    (w-foreign-values (rect :int16 0 :int16 0 :uint16 1 :uint16 1)
      (check (fill-rectangles *conn* OP-SRC picture abgr64 1 rect)))
    (check (free-pixmap *conn* pixmap));; since it never changes?
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
;; create an abgr64  from a list containing (red green blue)
(defun rgblist->64 (rgblist)
   (logior #xFFFF000000000000
	     (ash (the (unsigned-byte 8) (first rgblist))   8)
	     (ash (the (unsigned-byte 8) (second rgblist)) 24)
	     (ash (the (unsigned-byte 8) (third rgblist))  40)))

;; some basic pens
(defparameter *pen-black* nil)
(defparameter *pen-white* nil)
(defparameter *pen-trans* nil)
(defparameter *pen-green* nil)

(defun init-pens ()
  (setf *pen-black* (make-pen #xFFFF000000000000)
	*pen-white* (make-pen #xFFFFFFFFFFFFFFFF)
	*pen-trans* (make-pen #x0000000000000000)
	*pen-green* (make-pen #xFFFF0000FFFF0000)))
