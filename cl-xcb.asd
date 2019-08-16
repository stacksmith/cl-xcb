;;
(asdf:defsystem  #:cl-xcb 
  :serial t
  :depends-on (#:bordeaux-threads #:cffi #:cffi-libffi #:cl-freetype2)
  :components ((:file "package")
	       (:file "global")
	       (:file "lot")
	       (:file "bindings/libs")
	       (:file "bindings/xcb")
	       (:file "bindings/icccm")
	       (:file "bindings/events")
	       (:file "bindings/xcb-xrender")
	       (:file "bindings/xcb-util")
	       ;; Layer1 provides some helpful services
	       (:file "Layer1/keys")  ;key events are dealt with.
       	       (:file "Layer1/event-handling")
	       (:file "Layer1/pens")
	       (:file "Layer1/ft2")
	       
;;	       (:file "win/fonts")
;;	       (:file "win/styles")
;;	       (:file "win/chunks")
;;	       (:file "win/xbuf")
	       

;;	       (:file "win/xcb-system")
;;	       (:file "win/panels") 
;;	       (:file "win/win-base")
;;	       (:file "win/win-direct")
;;	       (:file "win/win-test")

	       (:file "Layer2/fonts")
	       (:file "Layer2/xcb-setup")
	       (:file "Layer2/win-registry")
	       (:file "Layer2/geo")
	       (:file "Layer2/xwin-util")
	       (:file "Layer2/event-subsystem")
	       (:file "Layer2/protowin")
	       
	       
;;	       (:file "display/attributes")
;;	       (:file "display/xbuf")
;;	       (:file "display/panel")
;;	       (:file "display/display")
	 

	        ))
