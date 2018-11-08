(in-package :xcb)
;;=============================================================================
;; Fonts
;;
;; Some basic fonts to get started
(defparameter *font-path-normal* "fonts/DejaVuSansMono.ttf")
(defparameter *font-path-bold* "fonts/DejaVuSansMono-Bold.ttf")
(defparameter *font-path-prop* "fonts/DejaVuSans.ttf")
(defparameter *font-normal* nil)
(defparameter *font-bold* nil)
(defparameter *font-prop* nil)

(defparameter *fonts* (make-array 256 :initial-element nil))

(defconstant  +font-normal+ 0)
(defconstant  +font-bold+ 1)
;;=============================================================================
(defun init-fonts ()
  (ft2init)
  (setf *font-normal*
	(make-font
	 :path(asdf:system-relative-pathname 'cl-xcb *font-path-normal*)
	 :w 640
	 :h 640)
	*font-bold*
	(make-font
	 :path (asdf:system-relative-pathname 'cl-xcb *font-path-bold*)
	 :w 640
	 :h 640)

	*font-prop*
	(make-font
	 :path (asdf:system-relative-pathname 'cl-xcb *font-path-prop*)
	 :w 640
	 :h 640))
  (ft2::get-loaded-advance (font-face *font-normal*) nil)

  (setf (aref *fonts* +font-normal+) *font-normal*
	(aref *fonts* +font-bold+) *font-bold*)
  )
 
