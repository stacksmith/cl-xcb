(in-package :xcb)

;;================================================================================
#|| GEO

Geometry of screen objects is represented by a rectangle that uses parent coords.

||#
(defstruct rect
  (x 0 :type S16)
  (y 0 :type S16)
  (w 0 :type U16)
  (h 0 :type U16))


(defmacro rect-translate (r h v)
  `(let ((r ,r))
     (incf (rect-x r) ,h)
     (incf (rect-y r) ,v)
     r))

(defun rect-moveto (r x y)
  (setf (rect-x r) x
	(rect-y r) y)
  r)
(defun rect-inset (r left &optional (top left) (right left) (bottom top))
  (with-slots (x y w h) r
    (incf x left) (incf y top)
    (decf w (+ left right))
    (decf h (+ top bottom))
    r)
  )


(defun rect-same-origin? (r x y)
  (and (= (rect-x r) x)
       (= (rect-y r) y)))

(defun rect-same-extent? (r w h)
  (and (= (rect-w r) w)
       (= (rect-h r) h)))

(defmacro in-rect ((name rect) &body body)
  `(let ((,name ,rect))
     (with-slots ((x. x) (y. y) (w. w) (h. h)) ,name
       ,@body)))

(defun rect-set (r x y w h)
  (in-rect (r r)
    (setf x. x y. y w. w h. h )))

(defun print-rect (o s)
  (in-rect (r o)
    (format s "(~A,~A)~A,~A"      x. y. w. h.)))

(defmethod print-object ((o rect) s)
  (print-unreadable-object (o s :type t )
    (print-rect o s)))

;;==============================================================================
(defstruct (pane (:include rect))
  )

(defstruct (container (:include pane))
  payload ;a list of contained objects
  )
