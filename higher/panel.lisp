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
;;
;; A panel is a visual object occupying space in a window.
;;
(defstruct (panel)
  ;; geometry
  (x 0 :type U32) (y 0 :type U32)
  (w 0 :type U32) (h 0 :type U32)
  (dad nil :type t )
  (fixed nil :type t)
    ;; margins
  (ml  10   :type U32)  (mr  10  :type U32)
  (mt  10   :type U32)  (mb 10  :type U32))

(defmacro in-panel ((name panel) &body body)
  `(let ((,name ,panel))
    
     (with-accessors ((x. panel-x) (y. panel-y) (w. panel-w) (h. panel-h)
		      (layout. panel-layout)
		      (mr. panel-mr) (ml. panel-ml)
		      (mt. panel-mt) (mb. panel-mb)
		      (dad. panel-dad) (fixed. panel-fixed)
		      ) ,name
       (symbol-macrolet ((max-x (- w. mr.))
			 (min-x ml.)
			 (max-y (- h. mt.))
			 (min-y mt.)
			 (work-width (- w. mr. ml.))
			 (work-height (- h. mb. mt.))
			 )
	 ,@body))))
;; -----------------------------------------------------------------------------
(defmethod print-object ((o panel) s)
  (print-unreadable-object (o s :type t )
    (in-panel (panel o)
      (format s "(~A,~A) ~Ax~A; [~A-~A ~A|~A]"
	      x. y. w. h.
	      ml. mr. mt. mb.))))
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
(defstruct (layout (:include panel))
  (payload (make-array 4 :adjustable t :fill-pointer 0)))

(defmacro in-layout ((name layout) &body body)
  `(let ((,name ,layout))
     (in-panel (,name ,name)
       (with-accessors ((payload. layout-payload)) ,name
	 (symbol-macrolet ((idx. (fill-pointer payload.))
			   (panel-count (fill-pointer payload.)))
	   ,@body)))))
;; -----------------------------------------------------------------------------
(defmethod print-object ((o layout) s)
  (print-unreadable-object (o s :type t )
    (in-layout (layout o)
      (format s "(~A,~A) ~Ax~A; [~A-~A ~A|~A] with:~%~A"
	      x. y. w. h.
	      ml. mr. mt. mb.
	      payload.))))
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
      (panel-detach (vector-pop payload.) layout))))
;;==============================================================================
;;
(defstruct (hlayout (:include layout)))
(defstruct (vlayout (:include layout)))

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
