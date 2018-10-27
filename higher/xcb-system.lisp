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
	  +WM-DELETE-WINDOW+ (easy-atom c "WM_DELETE_WINDOW"))))



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

;;=============================================================================
;; Fonts
(defparameter *font-path-normal* "fonts/DejaVuSansMono.ttf")
(defparameter *font-path-bold* "fonts/DejaVuSansMono-Bold.ttf")
(defparameter *font-normal* nil)
(defparameter *font-bold* nil)

;;------------------------------------------------------------------------------
;; Dispatch expose events via generic win-on-expose
(defun on-expose (event)
  (with-foreign-slots ((window x y width height count) event (:struct ES-EXPOSE))
   ;; (format t "ON-EXPOSE; count ~A.  ~A ~A ~A ~A~&" count x y width height)
    (win-on-expose (lisp-window window) event)))

;;------------------------------------------------------------------------------
;; RESIZE       - does not seem to work?
;;
(defun on-resize-request (event)
  (with-foreign-slots ((window width height) event (:struct ES-RESIZE-REQUEST))
    (win-on-resize (lisp-window window) width height)))

;;------------------------------------------------------------------------------
;; Handle window closure, right here for now.
(defun on-client-notify (e)
;;  (format t "on-client-notify")
  (with-foreign-slots ((window type data) e (:struct ES-CLIENT-MESSAGE))
    (when (and (= type +WM-PROTOCOLS+ )
	       (= data +WM-DELETE-WINDOW+))
      (check (destroy-window c window))
      (flush c)
      ;;(destroy (lisp-window window))
      t)))
;;------------------------------------------------------------------------------
;;
(defun on-destroy-notify (e)
;;  (format t "on-destroy-notify")
  (with-foreign-slots ((window ) e (:struct ES-DESTROY-NOTIFY))
     (destroy (lisp-window window))
    t))

(defmethod win-on-key-press ((win t) key state))

(defun on-key-press (e)
  (with-foreign-slots ((detail state event) e (:struct ES-INPUT))
;;    (format t "KEYCODE ~X ~A state ~X ~&" detail detail state)
;;    (format t "WINDOW ~A ~&" event)
    (win-on-key-press (lisp-window event) detail state))
  t
  )
(defun on-configure-notify (e)
   (with-foreign-slots (( window x y width height border-width response-type ) e (:struct ES-CONFIGURE-NOTIFY))
;;    (format t "CONF ~A ~&" response-type)
    (when (= 150 response-type)
      (win-on-configure-notify (lisp-window window) e x y width height))
    t))
#||
(defun on-resize-notify (e)
  (with-foreign-slots (( window x y width height border-width response-type ) e (:struct ES-CONFIGURE))
    (when (= 150 response-type)
      (win-on-configure-notify (lisp-window window) e x y width height))
    t))
||#


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
;; INIT
;; session-global initialization...
(defun init-fonts ()
  (ft2init)
  (setf *font-normal*
	(make-instance
	 'font :path
	 (asdf:system-relative-pathname 'cl-xcb *font-path-normal*)
	 :size 10)
	*font-bold*
	(make-instance
	 'font :path
	 (asdf:system-relative-pathname 'cl-xcb *font-path-bold*)
	 :size 10))
  (ft2::get-loaded-advance (face *font-normal*) nil) )


(defun init ()
  (init-xcb)
  (init-event-subsystem) ;; see event-handling.lisp
  (init-fonts)
  (init-pens) ;; attributes.lisp
  )





