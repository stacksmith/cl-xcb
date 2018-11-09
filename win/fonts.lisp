(in-package :xcb)
;;=============================================================================
;; Fonts
;;
;; Some basic fonts to get started
(defparameter *font-subdir* "fonts/")
(defparameter *font-name-normal* "fonts/DejaVuSansMono.ttf")
(defparameter *font-name-bold* "fonts/DejaVuSansMono-Bold.ttf")
(defparameter *font-name-prop* "fonts/DejaVuSans.ttf")
(defparameter *font-normal* nil)
(defparameter *font-bold* nil)
(defparameter *font-prop* nil)

(defparameter *fonts* (make-array 8 :adjustable t :fill-pointer 0))

(defconstant  +font-normal+ 0)
(defconstant  +font-bold+ 1)
(defconstant  +font-prop+ 2)

;;=============================================================================
(defun init-font (fontname w h)
  (make-font :path(asdf:system-relative-pathname
		   'cl-xcb (concatenate 'string *font-subdir* fontname))
	     :w w
	     :h h))

(defun init-fonts ()
  (ft2init)
  (setf *font-normal* (init-font "DejaVuSansMono.ttf" 640 640)
	*font-bold* (init-font "DejaVuSansMono-Bold.ttf" 640 640)
	*font-prop* (init-font "DejaVuSans.ttf" 640 640))

  (vector-push *font-normal* *fonts*)
  (vector-push *font-bold* *fonts*)
  (vector-push *font-prop* *fonts*)

  )
 
