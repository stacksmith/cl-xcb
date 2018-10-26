(in-package :xcb)


(defparameter *w* nil)

(defgeneric destroy (win))
;;-----------------------------------------------------------------------------
;; win protocol
(defgeneric win-on-configure-notify (win event x y w h))
(defgeneric win-on-expose (win event))
(defgeneric win-on-resize (win w h))
(defgeneric win-on-key-press (win key state))
;; simply redraw the window
(defgeneric win-refresh (win))
;;=============================================================================
;; Bufwin is a generic window with an off-screen buffer.
(defclass window-direct ()
  ((id  :accessor id :initform nil)  ;; window xcb id
   (pic :accessor pic :initform nil) ;; draw here picture id
   (width   :accessor width   :initform 0)
   (height  :accessor height  :initform 0)
   (xpos :accessor xpos :initform 0) ;; useful to differentiate resizing 
   (ypos :accessor ypos :initform 0) ;; from window motion
   (pic-lock :accessor pic-lock :initform nil)
   (gc       :accessor gc       :initform nil)))

(defmethod initialize-instance :after ((win window-direct) &key w h )
  (setf *w* win)
  (with-slots (pic-lock width height) win
    (setf width w
	  height h)
    (win-startup win)
    (setf pic-lock (bt:make-lock))
    win))
;;==============================================================================
;; called by initialize-instance
(defun win-startup (win)
  (with-slots (id pic width height gc) win
    (setf id (generate-id c) ;; window id
	  pic (generate-id c) ;;picture id
	  gc (generate-id c))
    (with-foreign-slots ((root root-visual white-pixel black-pixel
			       root-depth) s (:struct screen-t))
      (w-foreign-values (vals
			 :uint32 black-pixel
			 :uint32 GRAVITY-NORTH-WEST ;; Leave contents on resize.
			 :uint32 (+ EVENT-MASK-EXPOSURE
				    EVENT-MASK-STRUCTURE-NOTIFY
				    ;;EVENT-MASK-RESIZE-REDIRECT
				    ;; EVENT-MASK-BUTTON-PRESS
				    EVENT-MASK-KEY-PRESS )) 
	(check (create-window c COPY-FROM-PARENt id
			      root
			      0 0 width height 10
			      WINDOW-CLASS-INPUT-OUTPUT
			      root-visual
			      (+ CW-BACK-PIXEL CW-BIT-GRAVITY  CW-EVENT-MASK   ) vals)))
      ;; windows get registered
      (window-register id win)
      ;; screen picture for window
      (check (create-picture c pic id +RGB24+ 0 (null-pointer)))
      (map-window c id)      
      (flush c)
      
      ;; ask WM to send us WM_DELETE_WINDOW message on go-away click
      (w-foreign-values (patom :uint32 +WM-DELETE-WINDOW+)
	(check (change-property c PROP-MODE-REPLACE id +WM-PROTOCOLS+
				4 32 1 patom)))
      ;; GC
      (w-foreign-values (vals :uint32 white-pixel :uint32 0)
	(check (create-gc c gc id (+ GC-FOREGROUND GC-GRAPHICS-EXPOSURES) vals)))
)))
(defmethod destroy ((win window-direct))
  (remhash (id win) windows ))

(defparameter *w* nil)

(defun win-set-name (win name)
  (let ((len (length name))
	(str (foreign-string-alloc name)))
    (w-foreign-values (buf :uint32 8)
      (check (change-property c PROP-MODE-REPLACE (id win) ATOM-WM-NAME
			      ATOM-STRING 8 len str)))
    (foreign-free str)))


;;==============================================================================
;; handle resizing
;;
;; On-configure-notify tracks window size/position.  While not officially doc'd
;; 2 consecutive invocations with identical x y w h imply that we are done
;; resizing.
;;
(defmethod win-on-configure-notify ((win window-direct) e x y w h)
  (with-slots (width height xpos ypos) win
    ;; map: 1=pos 2=size 3=both
    (let ((pos  (or (/= xpos x) (/= ypos y)))
	  (size (or (/= width  w) (/= height h))))
      (when pos
	(setf xpos x ypos y))
      (when size
;;	(format *q* "~%conf resize 0 ~A; (~A ~A) ~A  ~A; ."  win x y w h)
	(setf width w height h))
      (unless (or pos  size)
	
;;	(win-refresh win)
	(format *q* "~%conf final 0 ~A; (~A ~A) ~A  ~A; ."  win x y w h)
;;
;;	(with-foreign-slots ((x y width height count) event (:struct ES-CONFIGURE-NOTIFY)) )
	))
  ;;  (format *q* "~A" (cffi:convert-from-foreign e ' (:struct ES-CONFIGURE-NOTIFY)))
    t))
;;==============================================================================
;; Synthesized by win-on-configure-notify, not resize event (which we are not
;; redirecting!)
;;
(defmethod win-on-resize ((win window-direct) w h)
  (error "")
  t)
;;------------------------------------------------------------------------------
(defmethod win-on-expose ((win window-direct) event)
  (with-foreign-slots ((x y width height count) event (:struct ES-expose))
    (format *q* "~%On-expose: (~A,~A) ~A x ~A  count: ~A" x y width height count)
    (with-slots (xpos ypos width height) win
      (format *q* " vs ~A ~A ~A ~A~%" xpos ypos width height))
    
    (win-redraw win x y width height))
  
  t)
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
	      (pic win) +ARGB32+ (glyphset *font-normal*)
	      0 0 xbuflen xbuf))
      (flush c))))


;;==================================
;; Initialize with (init)  -- see xcb-system.lisp
;;
(defun wintest ()
  (make-instance 'window-direct :w 640 :h 480)
  (sleep 0.1)
  (events-process)(flush c)

  (test-out "Hello World" *w* 20 20 ))
