(in-package :xcb)
;;================================================================================
#|| EVENT-SUBSYSTEM

Layer1 provides an event dispatch protocol (see layer1/event-handling.lisp)

Here we add actual event handlers that crack the event structure, resolve the
window id to a lisp object, and dispatch the event to the appropriate object.

||#

;;================================================================================
;; WIN EVENT generic protocol
;;
(defgeneric win-on-configure-notify (win synth x y w h e))
(defgeneric win-on-client-message   (win type data0 e))
(defgeneric win-on-destroy-notify   (win)) 
(defgeneric win-on-expose           (win x y width height count event))
(defgeneric win-on-key-press        (win key state))

;; synthetic
(defgeneric win-destroy (win))
(defgeneric win-on-resize (win w h))

(defun init-event-subsystem ()
  ;; prepare the event subsystem
  (event-dispatch-reset)
  (event-set-handler EVENT-EXPOSE           'ev-on-expose)
  (event-set-handler EVENT-CLIENT-MESSAGE   'ev-on-client-message)
  (event-set-handler EVENT-KEY-PRESS        'ev-on-key-press)
  (event-set-handler EVENT-CONFIGURE-NOTIFY 'ev-on-configure-notify)
  (event-set-handler EVENT-RESIZE-REQUEST   'ev-on-resize-request)
  (event-set-handler EVENT-DESTROY-NOTIFY   'ev-on-destroy-notify))

;;================================================================================
;; EXPOSE
;;
;; Dispatch expose events via generic win-on-expose
(defun ev-on-expose (event )
  (with-foreign-slots ((window x y width height count) event (:struct ES-EXPOSE))
;;    (format t "~%----------EV-ON-EXPOSE; count ~A.  ~A ~A ~A ~A~&"  count x y width height)
    (win-on-expose (winreg-ref window) x y width height count event)))

;;================================================================================
;; RESIZE
;;
(defun ev-on-resize-request (event)
  (with-foreign-slots ((window width height) event (:struct ES-RESIZE-REQUEST))
    (format t "~%----------EV-ON-RESIZE-REQUEST~~!~~ ~A ~A" width height)
    (win-on-resize (winreg-ref window) width height)
    ))

;;================================================================================
;; CLIENT-NOTIFY
;;
;; Handle window closure, right here for now.
(defun ev-on-client-message (e)
  (format t "~%----------EV-ON-CLIENT-MESSAGE")
  (with-foreign-slots ((window type data0) e (:struct ES-CLIENT-MESSAGE))
    (win-on-client-message (winreg-ref window) type data0 e)))
;;================================================================================
;; KEY-PRESS
;
(defun ev-on-key-press (e)
  (with-foreign-slots ((detail state event) e (:struct ES-INPUT))
    (format t "~%----------EV-ON-KEY-PRESS")
;;    (format t "KEYCODE ~X ~A state ~X ~&" detail detail state)
;;    (format t "WINDOW ~A ~&" event)
    (win-on-key-press (winreg-ref event) detail state)))

;;================================================================================
;; DESTROY-NOTIFY
;;
(defun ev-on-destroy-notify (e)
  (format t "~%----------EV-ON-DESTROY-NOTIFY")
  (with-foreign-slots ((window ) e (:struct ES-DESTROY-NOTIFY))
    (win-on-destroy-notify (winreg-ref window))))
;;================================================================================
;; CONFIGURE
;;
;;https://tronche.com/gui/x/xlib/events/window-state-change/configure.html
;; * global events happen to send 2 for the final resize event...
(defun ev-on-configure-notify (e)
  (with-foreign-slots
      (( window event x y width height border-width
		response-type override-redirect) e (:struct ES-CONFIGURE-NOTIFY))
    ;; Bit 7 of response-type is set when the notification is global.
;;    (let ((*print-base* 16)) (format t "~%----------EV-ON-CONFIGURE-NOTIFY ~A "response-type))
    (let ((global (logbitp 7 response-type) ))
      (when global
;;	(format t "~%----------EV-ON-CONFIGURE-NOTIFY")
;;	(format t "~%win:~A ev:~A ~A ~A ~A ~A ~A  [~A]" window event x y width height border-width response-type)
	(win-on-configure-notify (winreg-ref window) response-type;only 0, non-global
				 x y width height
				 e))) ))
