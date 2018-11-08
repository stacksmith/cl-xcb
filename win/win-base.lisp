(in-package :xcb)


(defparameter *w* nil)

;;-----------------------------------------------------------------------------
;; win protocol

;;=============================================================================
;; Bufwin is a generic window with an off-screen buffer.
(defstruct (win-base (:include layout) (:conc-name win-) (:constructor make-win-base%))
  (id 0 :type U32)
  (moved nil :type t)  (resized nil :type t) 
  (gc 0 :type U32))




(defun make-win-base (w h &optional maker)
  (let ((win (make-win-base% :x1 0 :y1 0 :x2 w :y2 h
			     )))
    (format t "OK! ~A" win)
    (setf *w* win)
    (init-win win :maker maker )
    win))


;;-------------------------------------------------------------------------
;; called by window's initialize-instance to create the window.  This
;; default one.  Each window may have a different one specified at creation
;; This method ensures that the class win-make-xcb-window will be called.
(defmethod win-make-xcb-window ((win win-base))
  (in-layout (layout win)
    (with-foreign-slots ((root root-visual) s (:struct screen-t))
      (w-foreign-values (vals
			 ;;:uint32 black-pixel
			 :uint32 GRAVITY-NORTH-WEST ;; Leave contents on resize.
			 :uint32 (+ EVENT-MASK-EXPOSURE
				    EVENT-MASK-STRUCTURE-NOTIFY
				    ;;EVENT-MASK-RESIZE-REDIRECT
				    ;; EVENT-MASK-BUTTON-PRESS
				    EVENT-MASK-KEY-PRESS )) 
	(check (create-window c COPY-FROM-PARENT (win-id win)
			      root
			      x1. y1. width height 10
			      WINDOW-CLASS-INPUT-OUTPUT
			      root-visual
			      (+ ;;CW-BACK-PIXEL
			       CW-BIT-GRAVITY  CW-EVENT-MASK   ) vals))

	;; always on top
)
	)))
(defmethod init-win ((win win-base)  &key maker  &allow-other-keys)
 
  (format t "~%init-instance of ~A "win)
  (with-slots ( id gc ) win
    (with-foreign-slots ((root root-visual white-pixel black-pixel
			       root-depth) s (:struct screen-t))
      ;; WINDOW
      (setf id (generate-id c)) ;; window id
      (if maker
	  (funcall maker win)
	  (win-make-xcb-window win))
  
      
      ;; windows get registered
      (window-register id win)
      ;; Make it deletable
      (w-foreign-values (patom :uint32 +WM-DELETE-WINDOW+)
	(check (change-property c PROP-MODE-REPLACE id +WM-PROTOCOLS+				4 32 1 patom))
	)
      ;; GC
      (setf gc (generate-id c))
      (w-foreign-values (vals :uint32 white-pixel
			      :uint32 0)
	(check (create-gc c gc id (+ GC-FOREGROUND    GC-GRAPHICS-EXPOSURES) vals))
	))

    (map-window c id)
    (format t "mapped")
    (flush c)
    win))


;;==============================================================================
;; Panel protocol
;;
;; window is a topmost panel, so we do not do anything..
(defmethod panel-attached ((panel win-base) layout)
  )
(defmethod panel-detached ((panel win-base) layout)
  )

;;==============================================================================
;; 
(defmethod win-on-destroy-notify ((win win-base))
  (remhash (win-id win) windows ))

;;==============================================================================
;;
(defmethod win-on-client-notify ((win win-base) type data0 e)
  (when (and (= type +WM-PROTOCOLS+ )
	     (= data0 +WM-DELETE-WINDOW+))
    (win-destroy win)))

;;==============================================================================
;;  Called by win-on-client-notify when the close-box is clicked.  Also, you
;; can call this method to destroy a window!  This is a place for code to
;; check if the window can be closed, dialog box, save data, etc.
;;
;; Use win-on-destroy-notify to do actual bookkeeping
(defmethod win-destroy ((win win-base))
  (check (destroy-window c (win-id win)))
  (flush c))


;;==============================================================================
;; handle resizing
;;
;; On-configure-notify tracks window size/position.  While not officially doc'd
;; there is a synthetic configure-notify with global x/y for resize, and for
;; move only that one is sent.
;;
;; So since the resize notification is never sent on my system, I monitor
;; CONFIGURE-NOTIFY messages with synth bit set (global).  If size or position
;; are different from before, I update the cached values.  If they are the same
;; we are done.  I call WIN-ON-RESIZE or WIN-ON-MOVE.  Which one, you may ask?
;; The reason we are here is because the size/position is same as the last time
;; So we have 2 flags: resized and moved.  Anytime we change size or position,
;; we set the flags; the final configure figures out which and calls the
;; on-resize or on-move func.
;;
;; TODO: does this work if someone calls ResizeWindow etc?  check...
;;
(defmethod win-on-configure-notify ((win win-base) synth wx wy ww wh e)
  (when synth
    (in-layout (layout win)
      (with-accessors ((resized. win-resized) (moved. win-moved)) win
      	(let ((size (or (/= width ww) (/= height wh)))
	      (pos  (or (/= x1. wx) (/= y1. wy))))
	  (when size
	    (setf x2. (+ x1. ww)
		  y2. (+ y1. wh))
	    (setf resized. t))
	  (when pos
	    (let ((w width)
		  (h height))
	      (setf x1. wx
		    y1. wy
		    x2. (+ wx w)
		    y2. (+ wy h)
		    moved. t)))
	  (unless (or pos size)
	    (when resized.
	      (win-on-resize win ww wh)
	      (setf resized. nil))
	    (when moved.
	      (win-on-move win wx wy)
	      (setf moved. nil))))))))
;;==============================================================================
;; Synthesized by win-on-configure-notify, not resize event (which we are not
;; redirecting!)
;;
(defmethod win-on-resize ((win win-base) w h)
 )
(defmethod win-on-move ((win win-base) x y)
 )
;;------------------------------------------------------------------------------
(defmethod win-on-expose ((win win-base) x y width height count event)
  ;;(clear-area c 0 (id win) x y width height)
  (win-redraw win x y width height)
  
  )
;;------------------------------------------------------------------------------
(defun win-redraw (win wx wy ww wh)
  (in-layout (layout win)
    (with-slots (gc id) win
      (check (clear-area c 0 id 0 0 width height))
      (w-foreign-values (vals :uint16 0 :uint16 0  :uint16 width :uint16 height)
	(check (poly-line c COORD-MODE-ORIGIN (win-id win) (win-gc win) 2 vals))
	
	;; ostensibly, we finished drawing
	(xcb::flush c)
	))))

;;=============================================================================
;; Utility functions
(defun win-set-name (win name)
  (let ((len (length name))
	(str (foreign-string-alloc name)))
    (w-foreign-values (buf :uint32 8)
      (check (change-property c PROP-MODE-REPLACE (win-id win) ATOM-WM-NAME
			      ATOM-STRING 8 len str)))
    (foreign-free str)))






;;==================================
;; Initialize with (init)  -- see xcb-system.lisp
;;
(defun wintest ()
  (make-win-base 640 480)
  (sleep 0.1)
  (events-process)(flush c)

 ;; (test-out "Hello World" *w* 20 20 )
  )
(defun always-on-top (win)
  ;;
  (let ((NET-WM-STATE       (easy-atom c "_NET_WM_STATE"))
	(NET-WM-STATE-ABOVE (easy-atom c "_NET_WM_STATE_ABOVE")))
    (w-foreign-values (event
		       :uint8  EVENT-Client-Message
		       :UINT8  32 ;ICCM
		       :UINT16 0  ;sequence
		       window-t (win-id win)
		       atom-t   NET-WM-STATE ;type
		       :UINT32 1 ;NET-WM-STATE-ADD
		       :UINT32 NET-WM-STATE-ABOVE
		       :UINT32 0
		       :UINT32 0
		       :UINT32 0)
      (check (send-event c 0 root-window
			 (logior EVENT-MASK-SUBSTRUCTURE-REDIRECT
				 EVENT-MASK-STRUCTURE-NOTIFY)
			 event))
      
      
      )))


