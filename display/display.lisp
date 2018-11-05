(in-package :xcb)
;;=============================================================================
;; Fonts
(defparameter *font-path-normal* "fonts/DejaVuSansMono.ttf")
(defparameter *font-path-bold* "fonts/DejaVuSansMono-Bold.ttf")
(defparameter *font-normal* nil)
(defparameter *font-bold* nil)

(defparameter *fonts* (make-array 256 :initial-element nil))
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
	 :h 640))
  (ft2::get-loaded-advance (font-face *font-normal*) nil)

  (setf (aref *fonts* 0) *font-normal*
	(aref *fonts* 1) *font-bold*)
  )


(defun init-display ()
  (init-fonts)
  (init-pens) ;; attributes.lisp
  (init-styles)
  )
