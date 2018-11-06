(in-package :xcb)

(defclass window (win-direct)
  (panels :accessor panels :initform nil))

(defun init-display ()
  (init-fonts)
  (init-pens) ;; attributes.lisp
  (init-styles)
  )
