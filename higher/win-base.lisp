(in-package :xcb)


(defparameter *w* nil)

;;-----------------------------------------------------------------------------
;; win protocol

;;=============================================================================
;; Bufwin is a generic window with an off-screen buffer.
(defclass win-base ()
  ((id     :accessor id     :initform nil)  ;; window xcb id
   (width  :accessor width  :initform 0)
   (height :accessor height :initform 0)
   (xpos   :accessor xpos   :initform 0) ;; useful to differentiate resizing
   (ypos   :accessor ypos   :initform 0) ;; from window motion
   (moved  :accessor moved  :initform nil) ;; useful to differentiate resizing
   (resized :accessor resized :initform 0) ;; from window motion
   
   (gc     :accessor gc     :initform nil)))
;;-------------------------------------------------------------------------
;; called by window's initialize-instance to create the window.  This
;; default one.  Each window may have a different one specified at creation
(defun win-make-window (win)
 )
;; This method ensures that the class win-make-xcb-window will be called.
(defmethod win-make-xcb-window ((win win-base))
  (with-slots (width height id) win
    (with-foreign-slots ((root root-visual) s (:struct screen-t))
      (w-foreign-values (vals
			 ;;:uint32 black-pixel
			 :uint32 GRAVITY-NORTH-WEST ;; Leave contents on resize.
			 :uint32 (+ EVENT-MASK-EXPOSURE
				    EVENT-MASK-STRUCTURE-NOTIFY
				    ;;EVENT-MASK-RESIZE-REDIRECT
				    ;; EVENT-MASK-BUTTON-PRESS
				    EVENT-MASK-KEY-PRESS )) 
	(check (create-window c COPY-FROM-PARENT id
			      root
			      0 0 width height 10
			      WINDOW-CLASS-INPUT-OUTPUT
			      root-visual
			      (+ ;;CW-BACK-PIXEL
				 CW-BIT-GRAVITY  CW-EVENT-MASK   ) vals))
	))))
(defmethod initialize-instance :after ((win win-base)
				       &key w h
					 maker 
					 &allow-other-keys)
  (setf *w* win)
  (format t "~%init-instance of ~A "win)
  (with-slots ( id gc width height) win
    (setf width w
	  height h)
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
;; 
(defmethod win-on-destroy-notify ((win win-base))
  (remhash (id win) windows ))

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
  (check (destroy-window c (id win)))
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
(defmethod win-on-configure-notify ((win win-base) synth x y w ht e)
   (when synth
      (with-slots (width height xpos ypos resized moved) win
	;; map: 1=pos 2=size 3=both
	(let ((size (or (/= width  w) (/= height ht)))
	      (pos  (or (/= xpos x) (/= ypos y))))
	  (when size (setf width w height ht resized t))
	  (when pos  (setf xpos x  ypos y moved t))
	  (unless (or pos size)
	    (when resized
	      (win-on-resize win w ht)
	      (setf resized nil))
	    (when moved
	      (win-on-move win x y)
	      (setf moved nil))))
	t)))
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
  (with-slots (width height gc id) win
    (clear-area c 0 id wx wy ww wh)
    (w-foreign-values (vals :uint16 wx :uint16 wy  :uint16 (+ wx ww) :uint16 (+ wy  wh))
      (check (poly-line c COORD-MODE-ORIGIN (id win) (gc win) 2 vals))
       
      ;; ostensibly, we finished drawing
      (xcb::flush c)
      )))

;;=============================================================================
;; Utility functions
(defun win-set-name (win name)
  (let ((len (length name))
	(str (foreign-string-alloc name)))
    (w-foreign-values (buf :uint32 8)
      (check (change-property c PROP-MODE-REPLACE (id win) ATOM-WM-NAME
			      ATOM-STRING 8 len str)))
    (foreign-free str)))



;;=============================================================================
;; String output
(defun test-out (string win x y &optional (pen *pen-white*))
  (let* ((slen (length string))
	 (xbuflen (+ (ash slen 2) 8))) ;32 bits per char + head
    (with-foreign-object (xbuf :uint8 xbuflen)
      (setf (mem-ref xbuf :UINT32 0) slen ;composite character count
	    (mem-ref xbuf :UINT16 4) x 
	    (mem-ref xbuf :UINT16 6) y ) 
      ;; set the glyphs
      (loop for i from 8 by 4
	 for c across string do
	   (setf (mem-ref xbuf :UINT32 i)
		 (glyph-assure *font-normal* (char-code c))))
      (check (composite-glyphs-32
	      c OP-OVER (pen-pic pen)
	      (pic win) +ARGB32+ (font-glyphset *font-normal*)
	      0 0 xbuflen xbuf))
      (flush c))))


;;==================================
;; Initialize with (init)  -- see xcb-system.lisp
;;
(defun wintest ()
  (make-instance 'win-base :w 640 :h 480)
  (sleep 0.1)
  (events-process)(flush c)

 ;; (test-out "Hello World" *w* 20 20 )
  )
