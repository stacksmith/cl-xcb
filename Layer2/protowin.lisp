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



(defstruct (win (:include container) (:constructor make-win%))
  (id 0 :type U32)
  ;; In order to track resizing, and moving, keep old geometry.
  oldw oldh
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
		        :uint32 0 ; black-pixel; - makes for no expose messages!
			    :uint32 GRAVITY-NORTH-WEST ;; Leave contents on resize.
			    :uint32 (+ ;;EVENT-MASK-EXPOSURE
				     EVENT-MASK-STRUCTURE-NOTIFY
				     ;;EVENT-MASK-RESIZE-REDIRECT
				     EVENT-MASK-BUTTON-PRESS
				     EVENT-MASK-KEY-PRESS))
      (in-rect (q win)
	(check (create-window *conn* COPY-FROM-PARENT (win-id win)
			      root
			      x. y. w. h. 10
			      WINDOW-CLASS-INPUT-OUTPUT
			      root-visual
			      (+ CW-BACK-PIXEL
				 CW-BIT-GRAVITY CW-EVENT-MASK)
			      vals))))))

;; TODO: check sequencing and consider error handling and memory issues...
(defun make-win (w h &optional (maker #'win-create-window-basic))
  ;; Create the Lisp win skeleton... Start with the GEOmetry...
  (let ((win (make-win% :x 0 :y 0 :w w :h h :oldw w :oldh h)))
    (setf *w* win)
    (with-slots (id gc pic) win
      (winreg-register  (setf id (generate-id *conn*))  win)
      (funcall maker win)
      ;; GC
       (setf gc (generate-id *conn*))
       (w-foreign-values (vals :uint32 #xFFFFFFFF :uint32 0)
	 (check (create-gc *conn* gc id (+ GC-FOREGROUND GC-GRAPHICS-EXPOSURES) vals)))
       (setf pic (generate-id *conn*))
       (check (create-picture *conn* pic id +RGB24+ 0 (null-pointer)))   

       (xwin-fix-deletable id)
       (map-window *conn* id))
    ;;should we flush?
    (flush *conn*)
    win))


  


(defun win-redraw (win)
  (in-rect (q win)
    (format t "~%WIN-REDRAW, clearing  ~A ~A ~A ~A" x. y. w. h.)
    ;; (clear-area *conn* 0 (win-id win) x. y. w. h.)
     (w-foreign-values (vals :uint16 0 :uint16 0  :uint16 w. :uint16 h.)
       (check (poly-line *conn* COORD-MODE-ORIGIN (win-id win) (win-gc win) 2 vals)))
     ;; ostensibly, we finished drawing
     (comp-string (win-pic win) (+ 3 0) (+ 12 0) (pen-pic *pen-white*)
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
;  (win-redraw win x y width height)
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
;; Called for move and resize... ww/wh= new width and height.
;; We also use oldw and oldh cache in window.  Normally same as window w/h.
;; * if oldw/oldh = ww/wh, final resize!
;; 
;; 
;;
(defmethod win-on-configure-notify ((win win) synth wx wy ww wh e)
  
  (with-slots (x y w h oldw oldh ) win 
 ;;   (format t "~%WIN-ON-CONFIGURE-NOTIFY ~A ~A ~A ~A ~A (~A ~A)"synth wx wy ww wh oldw oldh)
    (when (or (/= ww w)(/= wh h)) ;resized from original w/h ?
      (when (and (= ww oldw)(= wh oldh))
	(setf w oldw h oldh) ;; update w/h only at end of resize
	(win-do-resize win)))
    ;; Always update temporary w/h, and x/y in case we moved...
    (setf oldw ww  oldh wh	  x wx     y wy)))

;;==============================================================================
;; 
(defmethod win-on-destroy-notify ((win win))
  (format t "~%WIN-ON-DESTROY-NOTIFY. ~A" win )
  (winreg-unregister (win-id win))
  )

;;==============================================================================
;; Configure-notify events accumulate resize requests and update w/h
(defun  win-do-resize (win)
  (in-rect (r win)
    (format t "~%WIN-do-RESIZE ~A ~A" w. h.  ))
  
  
;;  (in-geo (q win)    (setf x2. w	  y2. h))
  (win-redraw win)
  )
