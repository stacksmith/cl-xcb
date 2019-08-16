(in-package :xcb)

;;================================================================================
#|| GEO

Geometry of screen objects is represented by a rectangle that uses parent coords.

||#
(defstruct geo
  (x1 0 :type S32)
  (y1 0 :type S32)
  (x2 0 :type S32)
  (y2 0 :type S32))

(defmacro in-geo ((name geo) &body body)
  `(let ((,name ,geo))
     (with-slots ((x1. x1) (y1. y1) (x2. x2) (y2. y2)) ,name
       (symbol-macrolet ((width.  (- x2. x1.))
			 (height. (- y2. y1.))
			 (this-geo ,name))
	 (macrolet ((translate (x y)
		      `(let ((x ,x) (y ,y))
			 (incf x1. x) (incf x2. x) 
			 (incf y1. y) (incf y2. y)
			 this-geo))
		    (inset (left &optional (top left) (right left) (bottom top))
		      `(progn (incf x1. ,left) (decf x2. ,right)
			      (incf y1. ,top) (decf  y2. ,bottom)
			      this-geo)))
	   ,@body)))))

(defun print-geo (o s)
  (in-geo (geo o)
    (format s "(~A,~A)(~A,~A)"      x1. y1. x2. y2.)))

(defmethod print-object ((o geo) s)
  (print-unreadable-object (o s :type t )
    (print-geo o s)))

;;==============================================================================
(defstruct (pane (:include geo))
  )

(defstruct (container (:include pane))
  payload ;a list of contained objects
  )
