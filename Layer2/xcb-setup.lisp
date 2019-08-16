(in-package :xcb)
;; Global XCB data

(defparameter *conn* nil) ;; xcb connection
(defparameter *setup* nil) ;; xcb setup
;;(defparameter w nil)
(defparameter root-window nil)
(defparameter +RGB24+ nil)
(defparameter +ARGB32+ nil)

(defparameter +WM-PROTOCOLS+ nil)
(defparameter +WM-DELETE-WINDOW+ nil)

(defparameter +NET-WM-STATE+ nil)
(defparameter +NET-WM-STATE-ABOVE+ nil)
(defparameter +NET-WM-STATE-BELOW+ nil)

(defparameter *w* nil)
;;=============================================================================
;; Global initialization
;;
;; Set up connection; get root data, pixel formats.  Prepare protocol atoms.
(defun init-xcb ()
  (setf *conn* (connect (null-pointer)(null-pointer)))
  (setf *setup* (getf (setup-roots-iterator (get-setup *conn*)) 'data))
  (setf root-window (mem-ref *setup* :uint32))
  (let ((formats (util-query-formats *conn*)))
    (setf +RGB24+ (mem-ref (util-find-standard-format
			  formats PICT-STANDARD-RGB-24) :uint32)
	  +ARGB32+ (mem-ref (util-find-standard-format
			   formats PICT-STANDARD-ARGB-32) :uint32)
	  +WM-PROTOCOLS+ (easy-atom *conn* "WM_PROTOCOLS")
	  +WM-DELETE-WINDOW+ (easy-atom *conn* "WM_DELETE_WINDOW")
)))

;; TODO: move to a top init file
(defun init ()
  (init-xcb)       
  (init-pens)
  (init-fonts)
  (init-winreg)
  (init-event-subsystem)
  )
