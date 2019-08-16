(in-package :xcb)
;;=============================================================================
;; Fonts
;;
;; Some basic fonts to get started


(defparameter *font-normal* nil)
(defparameter *font-bold* nil)
(defparameter *font-prop* nil)

(defparameter *fonts* (make-array 8 :adjustable t :fill-pointer 0))

(defconstant  +font-normal+ 0)
(defconstant  +font-bold+ 1)
(defconstant  +font-prop+ 2)


(defun init-fonts ()
  (ft2init)
  (setf *font-normal* (make-font "dejavu/DejaVuSansMono.ttf" 640 640)
	*font-bold* (make-font "dejavu/DejaVuSansMono-Bold.ttf" 640 640)
	*font-prop* (make-font "dejavu/DejaVuSans.ttf" 640 640))

  (vector-push *font-normal* *fonts*)
  (vector-push *font-bold* *fonts*)
  (vector-push *font-prop* *fonts*)

  )
 
