(in-package :xcb)
;;
;; XCB-SYSTEM
;;
;; This ties it all together: event-handling subsystem resolves window handles
;; to lisp objects and dispatches; initialization, etc.
;;=============================================================================
;; Global XCB data

(defparameter c nil) ;; xcb connection
(defparameter s nil) ;; xcb setup
;;(defparameter w nil)
(defparameter root-window nil)
(defparameter +RGB24+ nil)
(defparameter +ARGB32+ nil)

(defparameter +WM-PROTOCOLS+ nil)
(defparameter +WM-DELETE-WINDOW+ nil)

(defparameter +NET-WM-STATE+ nil)
(defparameter +NET-WM-STATE-ABOVE+ nil)
(defparameter +NET-WM-STATE-BELOW+ nil)
;;-----------------------------------------------------------------------------
;; win protocol
(defgeneric win-on-configure-notify (win synth x y w h e))
(defgeneric win-on-client-notify  (win type data0 e))
(defgeneric win-on-destroy-notify (win)) 
(defgeneric win-on-expose (win x y width height count event))
(defgeneric win-on-resize (win w h))
(defgeneric win-on-resize (win x y))
(defgeneric win-on-key-press (win key state))

;; synthetic
(defgeneric win-destroy (win))


;;=============================================================================
;; Global initialization
;;
;; Set up connection; get root data, pixel formats.  Prepare protocol atoms.
(defun init-xcb ()
  (setf c (connect (null-pointer)(null-pointer)))
  (setf s (getf (setup-roots-iterator (get-setup c)) 'data))
  (setf root-window (mem-ref s :uint32))
  (let ((formats (util-query-formats c)))
    (setf +RGB24+ (mem-ref (util-find-standard-format
			  formats PICT-STANDARD-RGB-24) :uint32)
	  +ARGB32+ (mem-ref (util-find-standard-format
			   formats PICT-STANDARD-ARGB-32) :uint32)
	  +WM-PROTOCOLS+ (easy-atom c "WM_PROTOCOLS")
	  +WM-DELETE-WINDOW+ (easy-atom c "WM_DELETE_WINDOW")
)))



;;==========================================================================
;; window registry
;;
;; global windows hashtable
(defparameter windows (make-hash-table))
;;
;;------------------------------------------------------------------------------
(defun window-register (id object)
  (setf (gethash id windows) object) )
;;------------------------------------------------------------------------------
(defun window-unregister (id)
  (remhash id windows))
;;------------------------------------------------------------------------------
;; retreive window's lisp object
(defun lisp-window (id)
  (gethash id windows))

;;(declaim (inline window-object window-register window-unregister))



;;------------------------------------------------------------------------------
;; Dispatch expose events via generic win-on-expose
(defun ev-on-expose (event )
  (with-foreign-slots ((window x y width height count) event (:struct ES-EXPOSE))
   ;; (format t "ON-EXPOSE; count ~A.  ~A ~A ~A ~A~&" count x y width height)
    (win-on-expose (lisp-window window) x y width height count event)))

;;------------------------------------------------------------------------------
;; RESIZE       - does not seem to work?
;;
(defun ev-on-resize-request (event)
  (with-foreign-slots ((window width height) event (:struct ES-RESIZE-REQUEST))
    (format t "~%RESIZE~~!~~")
  ;;  (win-on-resize (lisp-window window) width height)
    ))

;;------------------------------------------------------------------------------
;; Handle window closure, right here for now.
(defun ev-on-client-notify (e)
;;  (format t "on-client-notify")
  (with-foreign-slots ((window type data0) e (:struct ES-CLIENT-MESSAGE))
    (win-on-client-notify (lisp-window window) type data0 e)))
;;------------------------------------------------------------------------------
;;
(defun ev-on-destroy-notify (e)
;;  (format t "on-destroy-notify")
  (with-foreign-slots ((window ) e (:struct ES-DESTROY-NOTIFY))
     (win-on-destroy-notify (lisp-window window))))


(defmethod win-on-key-press ((win t) key state))

(defun ev-on-key-press (e)
  (with-foreign-slots ((detail state event) e (:struct ES-INPUT))
;;    (format t "KEYCODE ~X ~A state ~X ~&" detail detail state)
;;    (format t "WINDOW ~A ~&" event)
    (win-on-key-press (lisp-window event) detail state))
  )
;;---------------------------------------------------------------------------
;;https://tronche.com/gui/x/xlib/events/window-state-change/configure.html
(defun ev-on-configure-notify (e)
  (with-foreign-slots (( window event x y width height border-width response-type override-redirect) e (:struct ES-CONFIGURE-NOTIFY))
    (win-on-configure-notify (lisp-window window) (logbitp 7 response-type) 
			     x y width height
			     e) ))
#||
(defun on-resize-notify (e)
  (with-foreign-slots (( window x y width height border-width response-type ) e (:struct ES-CONFIGURE))
    (when (= 150 response-type)
      (win-on-configure-notify (lisp-window window) e x y width height))
    t))
||#

;;=============================================================================
;; event system initialization.
;;
;; The intial dispatch table is patched with simple handlers that crack the
;; event structure as appropriate, lookup the Lisp window, and dispatch to
;; Lisp methods for the window class.
;;
(defun init-event-subsystem ()
  ;; prepare the event subsystem
  (event-dispatch-reset)
  (event-set-handler EVENT-EXPOSE           #'ev-on-expose)
  (event-set-handler EVENT-CLIENT-MESSAGE   #'ev-on-client-notify)
  (event-set-handler EVENT-KEY-PRESS        #'ev-on-key-press)
  (event-set-handler EVENT-CONFIGURE-NOTIFY #'ev-on-configure-notify)
  (event-set-handler EVENT-RESIZE-REQUEST   #'ev-on-resize-request)
  (event-set-handler EVENT-DESTROY-NOTIFY   #'ev-on-destroy-notify))
;;=============================================================================
;; create a picture
(defun new-offscreen-picture (width height
				 &optional (value-mask 0)
				   (value-list (null-pointer)))
  "return picture and pixmap ids"
  (with-ids (pixmap picture)
    (check (create-pixmap  c 32 pixmap root-window width height))
    (check (create-picture c picture pixmap +ARGB32+ value-mask value-list))
    (pic-rect picture #xFFFF000000000000 0 0 width height)
    (flush c)
    (values picture pixmap)))

(defun pic-rect (picture color x y width height)
  (w-foreign-values
      (rect :int16 x :int16 y :int16 width :uint16 height)
    (check (fill-rectangles c OP-OVER picture color 1 rect))))




;;=============================================================================
;; comp-string
(let ((xbuf (foreign-alloc :UINT8 :count (+ 1024 8) ))) ;; enough for 256 characters
  (defun comp-string (pic x y penpic string  &optional (start 0)
						   (end (length string)))
     ;; set the glyphs
    (let ((cnt (- end start)))
      (format t "~%~A ~A ~A" start end cnt) (force-output t)
      (setf (mem-ref xbuf :UINT32 0) cnt
	    (mem-ref xbuf :UINT16 4) x 
	    (mem-ref xbuf :UINT16 6) y )
      
      (loop for i from 8 by 4
	 for sindex from start below end 
	 for code = (char-code (char string sindex))
	 do (setf (mem-ref xbuf :UINT32 i) code)
	   (glyph-assure *font-normal* code )
	   )
      (dump xbuf)
      (check (composite-glyphs-32
	      c OP-OVER
	      penpic pic
	      +ARGB32+ (font-glyphset *font-normal*)
	      0 0 (+ 8 (* 4  cnt)) xbuf)))))

(defun init ()
  (init-xcb)
  (init-pens)
  (init-fonts)
  (init-event-subsystem) ;; see event-handling.lisp

;;  (init-attrs)
  )




;;==================================
;; Initialize with (init)  -- see xcb-system.lisp
;;
(defun test1 ()
  (make-win-direct 640 480;; :maker #'win-make-window1
		 )
  (sleep 0.1)
  (events-process)(flush c)

 )
;; (comp-string (win-pic *w*) 20 20 (pen-pic *pen-white*) "Hello World" (font-glyphset *font-normal*))

