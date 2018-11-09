(in-package :xcb)
;;
;; Panel
;;
;; A panel is a rectangle (with margin information) usually contained inside a
;; layout.  Layout notifies the panel of attachment and detachment.
(defun array-insert-element (array object at  )
  (vector-push-extend object array) ;;well, it extends array... clumsy, no?
  (when at
    (replace array array :start1 (1+ at) :start2 at)
    (setf (aref array at) object))
  object)

(defun array-remove-element (array at)
  (when at
    (replace array array :start1 at :start2 (1+ at)))
  (vector-pop array))

;;==============================================================================
;; GEO    Geometry
;;
;; Geometry of screen objects is represented by two points, upper left and
;; lower right.
(defstruct geo
  (x1 0 :type S16)
  (y1 0 :type S16)
  (x2 0 :type S16)
  (y2 0 :type S16))

(defmacro in-geo ((name geo) &body body)
  `(let ((,name ,geo))
     (with-accessors ((x1. geo-x1) (y1. geo-y1)
		      (x2. geo-x2) (y2. geo-y2)) ,name
       (symbol-macrolet ((width  (- x2. x1.))
			 (height (- y2. y1.))
			 (this-geo ,name))
	 (macrolet ((translate (x y)
		      `(let ((x ,x) (y ,y))
			 (incf x1. x) (incf x2. x)
			 (incf y1. y) (incf y2. y)))
		    (inset (left top right bottom)
		      `(progn (incf x1. ,left) (decf x2. ,right)
			      (incf y1. ,top) (decf  y2. ,bottom))))
	   ,@body)))))

(defun print-geo (o s)
  (in-geo (geo o)
    (format s "(~A,~A)(~A,~A)"      x1. y1. x2. y2.)))
(defmethod print-object ((o geo) s)
  (print-unreadable-object (o s :type t )
    (print-geo o s)))

;;==============================================================================
;;
;; A panel is a visual object occupying space in a window.
;;
(defstruct (panel (:include geo) (:constructor make-panel%))
  (dad nil :type t )
;;  (work (make-geo)  :type geo)
  )

(defmacro in-panel ((name panel) &body body)
  `(let ((,name ,panel))
     (in-geo (,name ,panel)
       (with-accessors (;;(work. panel-work)
			(dad. panel-dad) ;;(fixed. panel-fixed)
			) ,name
	 ;;(symbol-macrolet ())
	 ,@body))))

(defun make-panel (x1 y1 x2 y2 &optional)
  (make-panel% :x1 x1 :y1 y1 :x2 x2 :y2 y2
;;	       :work (make-geo :x1 x1 :y1 y1 :x2 x2 :y2 y2)
	       ))	




;; -----------------------------------------------------------------------------
(defun print-panel (o s)
  (in-panel (panel o)
    (print-geo o s) ;;(print-geo work. s)
    ))
(defmethod print-object ((o panel) s)
  (print-unreadable-object (o s :type t )
    (print-panel o s)))

;;==============================================================================
;; Panel protocol
;;
;; Panels are notified by the container as they are attached/detached.  The
;; panel should consider itself inactive when not attached.  Container first
;; adds the panel to its store.
;;
(defmethod panel-attached ((panel panel) layout)
  (setf (panel-dad panel) layout))

;; Container calls detach just after removing it its payload store;
(defmethod panel-detached ((panel panel) layout)
  (setf (panel-dad panel) nil))
;;==============================================================================
;; Container: a panel containing subpanels
;;
(defstruct (container (:include panel) (:constructor make-container%))
  (payload (make-array 4 :adjustable t :fill-pointer 0)))

(defmacro in-container ((name container) &body body)
  `(let ((,name ,container))
     (in-panel (,name ,name)
       (with-accessors ((payload. container-payload)) ,name
	 (symbol-macrolet ((idx. (fill-pointer payload.))
			   (panel-count (fill-pointer payload.)))
	   ,@body)))))
(defun make-container (x1 y1 x2 y2 )
  (make-container% :x1 x1 :y1 y1 :x2 x2 :y2 y2
;;		:work (make-geo :x1 x1 :y1 y1 :x2 x2 :y2 y2)
		))

;; -----------------------------------------------------------------------------
(defmethod print-object ((o container) s)
  (print-unreadable-object (o s :type t )
    (print-panel o s)
    (in-container (container o)
      (format s "with:~%~A"  payload.))))
;; -----------------------------------------------------------------------------
(defun container-insert (container panel index)
  (in-container (container container)
    (array-insert-element payload. panel index)
    (panel-attached panel container)
    (re-layout container)))
 

(defun container-find (container panel)
  (find panel (container-payload container)))

(defun container-remove-at (container index)
  (array-remove-element (container-payload container) index))

(defun container-remove-panel (container panel)
  (let ((index (container-find container panel)))
    (if index
	(progn
	  (container-remove-at container index)
	  (panel-detached panel container))
	(error ()))))

(defun container-clear (container)
  (in-container (container container)
    (dotimes (i panel-count)
      (panel-detached (vector-pop payload.) container))))
;;==============================================================================
;;





  

  

(defmethod re-layout ((panel panel))
  )
