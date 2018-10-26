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
	       (:file "bindings/keys")
	       
	       (:file "higher/ft2")
	       (:file "higher/event-handling")
	       (:file "higher/xcb-system")
	       (:file "higher/attributes")
	       (:file "higher/window-direct")
;;	       (:file "higher/window-double")
;;	       (:file "higher/geometry")
	     ;;  (:file "higher/lemwin")
	 

	        ))
