(in-package :xcb)

;;================================================================================
#|| PROTOWIN

Our goal is to minimize foreign interactions with X as they are full of opacity.
The strategy is to deal with a single toplevel X window, cope with the resizing
and event handling issues.  Internally, we can partition it as we wish.  This
allows us finer control over event dispatching, not to mention that we can 
control the events we are interested in internally.

So we shall concentrate on top-like windows here...  The two big concerns are
* RESIZING (which is kind of broken in X)
* EVENTS

This window is initialized with a black-pixel for CW-BACK-PIXEL, which means
no EXPOSE messages ever.  We just redraw on resize

||#
;;-------------------------------------------------------------------------
;; called by window's initialize-instance to create the window.  This
;; default one.  Each window may have a different one specified at creation
;; This method ensures that the class win-make-xcb-window will be called.



(defstruct (win (:include container) (:conc-name win-) (:constructor make-win%))
  (id 0 :type U32)
  ;; In order to track resizing, we note the w/h here.  When events are same,done.
  (oldw 0 :type fixnum)
  (oldh 0 :type fixnum)
  ;; GC - what good is a window if we don't draw into it...
  (gc nil)
  (pic nil)

  )
;;--------------------------------------------------------------------------------
;; WIN-CREATE-window-basic.
;;
;; Called by Lisp window constructor to create an actual X-WINDOWS window.
;; The only thing we have so far is the ID...
;; Note: this should not be a virtual function as we will do something entirely
;; different next time
(defun win-create-window-basic (win)
  (with-foreign-slots ((root root-visual) *setup* (:struct screen-t))
    (w-foreign-values (vals
		       :uint32 0;black-pixel - makes for no expose messages!
		       :uint32 GRAVITY-NORTH-WEST ;; Leave contents on resize.
		       :uint32 (+ EVENT-MASK-EXPOSURE
				  EVENT-MASK-STRUCTURE-NOTIFY
				  ;;EVENT-MASK-RESIZE-REDIRECT
				  ;; EVENT-MASK-BUTTON-PRESS
				  EVENT-MASK-KEY-PRESS ))
      (in-geo (q win)
	(check (create-window *conn* COPY-FROM-PARENT (win-id win)
			      root
			      x1. y1. width. height. 10
			      WINDOW-CLASS-INPUT-OUTPUT
			      root-visual
			      (+ CW-BACK-PIXEL
			       CW-BIT-GRAVITY  CW-EVENT-MASK   )
			      vals))))))


(defun make-win (w h &optional (maker #'win-create-window-basic))
  ;; Create the Lisp win skeleton... Start with the GEOmetry...
  (let ((win (make-win% :x1 0 :y1 0 :x2 w :y2 h)))
    (setf *w* win)
    (with-slots (id gc pic) win
      (winreg-register  (setf id (generate-id *conn*))  win)
      (funcall maker win)
      ;; GC
       (setf gc (generate-id *conn*))
       (w-foreign-values (vals :uint32 #xFFFFFFFF :uint32 0)
	 (check (create-gc *conn* gc id (+ GC-FOREGROUND
					  GC-GRAPHICS-EXPOSURES)
			   vals)))
       (setf pic (generate-id *conn*))
       (check (create-picture *conn* pic id +RGB24+ 0 (null-pointer)))   
       ;;should we flush?
       (xwin-fix-deletable id)
       (map-window *conn* id))
    (flush *conn*)
    win))


  


(defmethod win-redraw ((win win) x y w h)
   (in-geo (q win)
     (clear-area *conn* 0 (win-id win) 0 0 width. height.)
     (w-foreign-values (vals :uint16 0 :uint16 0  :uint16 width. :uint16 height.)
       (check (poly-line *conn* COORD-MODE-ORIGIN (win-id win) (win-gc win) 2 vals)))
     ;; ostensibly, we finished drawing
     (comp-string (win-pic win) (+ 3 x1.) (+ 12 y1.) (pen-pic *pen-white*)
		  (format nil "Hellow"))
     
     (xcb::flush *conn*)) )
;;==============================================================================
;; Event handlers
;;
;;------------------------------------------------------------------------------
;; ON-DESTROY
;; This is a synthetic event.  It is called before any destruction takes place,
;; so both Lisp and X objects are still alive... (rethink that, maybe?)
(defmethod win-on-destroy ((win win))
  (check (free-picture *conn* (win-pic win)))
   ;; this will trigger on-destroy
  (check (destroy-window *conn* (win-id win)))
  (flush *conn*)

  )


;;==============================================================================
;; EXPOSE
;;
(defmethod win-on-expose ((win win) x y width height count event)
  (format t "~%WIN-ON-EXPOSE.. ~A ~A ~A ~A ~A "x y width height count)
 
  (win-redraw win x y width height)
  
  )
;;==============================================================================
;; CLIENT-NOTIFY
(defmethod win-on-client-message ((win win) type data0 e)
  (format t "~%WIN-ON-CLIENT-MESSAGE. type: ~A data0: ~A  "type data0)
  (when (and (= type +WM-PROTOCOLS+ )
	     (= data0 +WM-DELETE-WINDOW+))
    ;; Call pre-destruction handler
    (win-on-destroy win)
    ))
;;==============================================================================
;; KEY-PRESS
(defmethod win-on-key-press((win win) key state)
  (format t "~%WIN-ON-KEY-PRESS: ~A ~A  " key state)
  )
;;==============================================================================
;; CONFIGURE-NOTIFY
;; * synth is always 0 for now... we do not care about global motion
;;
(defmethod win-on-configure-notify ((win win) synth wx wy ww wh e)
  (with-slots (oldw oldh) win
 ;;   (format t "~%WIN-ON-CONFIGURE-NOTIFY ~A ~A ~A ~A ~A "synth wx wy ww wh)
    ;; We only care about resizing...
    (when (logbitp 7 synth)
      (if (and (= oldw ww) (= oldh wh)) ;; did w and height remain same for 2?
       	  (win-on-resize win ww wh)
	  (setf oldw ww oldh wh))))
  )
;;==============================================================================
;; 
(defmethod win-on-destroy-notify ((win win))
  (format t "~%WIN-ON-DESTROY-NOTIFY. ~A" win )
  (winreg-unregister (win-id win))
  )

;;==============================================================================
;; ON-RESIZE  (faked from ON-CONFIGURE-NOTIFY!)
(defmethod win-on-resize ((win win) w h)
  (format t "~%WIN-ON-RESIZE (fake).  ~A ~A" w h )
  (in-geo (q win)
    (setf x2. w
	  y2. h))
  (win-redraw *w* 0 0 0 0)
  )
