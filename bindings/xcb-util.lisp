(in-package :xcb)


;;
(defxcb ("xcb_aux_clear_window" clear-window)
  (w    window-t)
  )


(defcfun ("xcb_aux_parse_color" parse-color%) :int
  (name :string)
  (red   (:pointer :UINT16))
  (green (:pointer :UINT16))
  (blue  (:pointer :UINT16))
  )



