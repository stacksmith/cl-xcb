(in-package :xcb)

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
			 (min-y mt.))
	 ,@body))))
;; -----------------------------------------------------------------------------
(defmethod print-object ((o panel) s)
  (print-unreadable-object (o s :type t )
    (in-panel (panel o)
      (format s "(~A,~A) ~Ax~A; [~A-~A ~A~A]"
	      x. y. w. h.
	      ml. mr. mt. mb.))))
;;==============================================================================
;; Layout: a panel containing subpanels
(defstruct (layout (:include panel))
  (payload (make-array 4 :adjustable t :fill-pointer 0)))

(defmacro in-layout ((name layout) &body body)
  `(let ((,name ,layout))
     (in-panel (,name ,name)
       (with-accessors ((payload. layout-payload)) ,name
	 (symbol-macrolet ((idx. (fill-pointer payload.))
			   (child-count (1- (fill-pointer payload.))))
	   ,@body)))))
;; -----------------------------------------------------------------------------
(defmethod print-object ((o layout) s)
  (print-unreadable-object (o s :type t )
    (in-layout (layout o)
      (format s "(~A,~A) ~Ax~A; [~A-~A ~A~A] with:~%~A"
	      x. y. w. h.
	      ml. mr. mt. mb.
	      payload.))))
;; -----------------------------------------------------------------------------
(defun panel-insert (panel layout index)
  (in-layout (layout layout)
    (let ((payload payload.))
      (when (< index (vector-push-extend panel payload))
	(replace payload payload :start1 index :start2 (1+ index))
	(setf (aref payload index) panel)))
    (setf (panel-dad panel) layout)
    (layout layout))
  )

(defun panel-append (panel layout)
  (in-layout (layout layout)
    (vector-push-extend panel payload.)
    (setf (panel-dad panel) layout))
  (layout layout)) 

(defun panel-prepend (panel layout)
  (panel-insert panel layout 0))

(defun layout-find (layout panel)
  (find panel (layout-payload layout)))

(defun layout-remove (layout panel)
  (let ((payload (layout-payload layout))
	(index (layout-find layout panel)))
    (if index
	(progn
	  (setf (panel-dad panel) nil)
	  (replace payload payload :start1 index :start2 (1+ index))
	  (in-layout (layout layout)
	    (decf idx.))
	  (layout layout))
	(error ())))
  )
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
