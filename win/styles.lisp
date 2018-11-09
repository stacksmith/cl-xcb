(in-package :xcb)
;;=============================================================================
;; Styles
;;
;; A style is a combination of a font along with a foreground and background
;; colors (represented as pens for maximum flexibility)
;;

(defstruct (style (:constructor make-style%))
  (font nil :type font)
  (fore nil :type pen)
  (back nil :type pen))

;; Make a style using an index into a font-table, and a newly-created fore and
;; background pens (to establish ownership)... 

(defun make-style (font-index fore64 back64)
  (make-style%
   :font (aref *fonts* font-index)
   :fore (make-pen fore64)
   :back (make-pen back64)))

 
(defparameter *styles* (make-array 8 :adjustable t :fill-pointer 0))

(defun style-add (font-index fore64 back64)
  (vector-push-extend (make-style font-index fore64 back64) *styles*))

(defun styles-init ()
  (style-add +font-normal+ 0 0)                 ;0 transparent space
  (style-add +font-normal+ #xFFFFFFFFFFFFFFFF 0) ;1 white
  (style-add +font-normal+ #xFFFF6262C1C12C2C 0) ;2 emerald text
  (style-add +font-normal+ #xFFFFFFFF00000000 0) ;3 blue
  (style-add +font-normal+ #xFFFF0F004300BD00 0) ;4 tia maria - string
  )

