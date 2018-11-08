;;
(asdf:defsystem  #:cl-xcb 
  :serial t
  :depends-on (#:bordeaux-threads #:cffi #:cffi-libffi #:cl-freetype2)
  :components ((:file "package")
	       (:file "global")
	       (:file "bindings/libs")
	       (:file "bindings/xcb")
	       (:file "bindings/icccm")
	       (:file "bindings/events")
	       (:file "bindings/xcb-xrender")
	       (:file "bindings/xcb-util")
	       (:file "bindings/keys")
	       
	       (:file "win/ft2")
	       (:file "win/pens")
	       (:file "win/event-handling")
	       (:file "win/xcb-system")
	       (:file "win/panels")
;;	       (:file "win/attributes")
	       (:file "win/win-base")
	       (:file "win/win-direct")
	       (:file "win/win-test")

	       (:file "display/attributes")
	       (:file "display/xbuf")
;;	       (:file "display/panel")
	       (:file "display/display")
	 

	        ))
