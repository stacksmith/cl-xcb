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

(defstruct pt2
  (x1 0 :type S16)
  (y1 0 :type S16)
  (x2 0 :type S16)
  (y2 0 :type S16))

(defmacro in-pt2 ((name pt2) &body body)
  `(let ((,name ,pt2))
     (with-accessors ((x1. pt2-x1) (y1. pt2-y1)
		      (x2. pt2-x2) (y2. pt2-y2)) ,name
       (symbol-macrolet ((width  (- x2. x1.))
			 (height (- y2. y1.))
			 (this-pt2 ,name))
	 (macrolet ((translate (x y)
		      `(let ((x ,x) (y ,y))
			 (incf x1. x) (incf x2. x)
			 (incf y1. y) (incf y2. y)))
		    (inset (left top right bottom)
		      `(progn (incf x1. ,left) (decf x2. ,right)
			      (incf y1. ,top) (decf  y2. ,bottom))))
	   ,@body)))))

(defun print-pt2 (o s)
  (in-pt2 (pt2 o)
    (format s "(~A,~A)(~A,~A)"      x1. y1. x2. y2.)))
(defmethod print-object ((o pt2) s)
  (print-unreadable-object (o s :type t )
    (print-pt2 o s)))

;;==============================================================================
;;
;; A panel is a visual object occupying space in a window.
;;
(defstruct (panel (:include pt2) (:constructor make-panel%))
  (dad nil :type t )
;;  (work (make-pt2)  :type pt2)
  )

(defmacro in-panel ((name panel) &body body)
  `(let ((,name ,panel))
     (in-pt2 (,name ,panel)
       (with-accessors (;;(work. panel-work)
			(dad. panel-dad) ;;(fixed. panel-fixed)
			) ,name
	 ;;(symbol-macrolet ())
	 ,@body))))

(defun make-panel (x1 y1 x2 y2 &optional)
  (make-panel% :x1 x1 :y1 y1 :x2 x2 :y2 y2
;;	       :work (make-pt2 :x1 x1 :y1 y1 :x2 x2 :y2 y2)
	       ))	




;; -----------------------------------------------------------------------------
(defun print-panel (o s)
  (in-panel (panel o)
    (print-pt2 o s) ;;(print-pt2 work. s)
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
;; Layout: a panel containing subpanels
(defstruct (layout (:include panel) (:constructor make-layout%))
  (payload (make-array 4 :adjustable t :fill-pointer 0)))

(defmacro in-layout ((name layout) &body body)
  `(let ((,name ,layout))
     (in-panel (,name ,name)
       (with-accessors ((payload. layout-payload)) ,name
	 (symbol-macrolet ((idx. (fill-pointer payload.))
			   (panel-count (fill-pointer payload.)))
	   ,@body)))))
(defun make-layout (x1 y1 x2 y2 )
  (make-layout% :x1 x1 :y1 y1 :x2 x2 :y2 y2
;;		:work (make-pt2 :x1 x1 :y1 y1 :x2 x2 :y2 y2)
		))

;; -----------------------------------------------------------------------------
(defmethod print-object ((o layout) s)
  (print-unreadable-object (o s :type t )
    (print-panel o s)
    (in-layout (layout o)
      (format s "with:~%~A"  payload.))))
;; -----------------------------------------------------------------------------
(defun layout-insert (layout panel index)
  (in-layout (layout layout)
    (array-insert-element payload. panel index)
    (panel-attached panel layout)
    (re-layout layout)))
 

(defun layout-find (layout panel)
  (find panel (layout-payload layout)))

(defun layout-remove-at (layout index)
  (array-remove-element (layout-payload layout) index))

(defun layout-remove-panel (layout panel)
  (let ((index (layout-find layout panel)))
    (if index
	(progn
	  (layout-remove-at layout index)
	  (panel-detached panel layout))
	(error ()))))

(defun layout-clear (layout)
  (in-layout (layout layout)
    (dotimes (i panel-count)
      (panel-detached (vector-pop payload.) layout))))
;;==============================================================================
;;



#||
;; -----------------------------------------------------------------------------
(defgeneric change-size (panel w h))
(defgeneric change-position (panel x y))
(defgeneric draw (panel win))
(defgeneric layout (panel &key &allow-other-keys))
;;(defgeneric on-key)

(defmethod change-size ((panel panel) w h)
  )
(defmethod change-position ((panel panel) x y)
  )

(defmethod draw ((panel panel) win)
  (in-panel (panel)
    (clear-area c 0 (id win) x. y. w. h.)
    (w-foreign-values (vals :uint16 min-x :uint16 min-y :uint16 max-x :uint16 max-y)
      (check (poly-line c COORD-MODE-ORIGIN (id win) (gc win) 2 vals))
      ;; ostensibly, we finished drawing
      (xcb::flush c) ;;TODO
      )))
||#

(defmethod re-layout ((panel panel))
  )
