(in-package :xcb)
;;================================================================================
#|| LAYER2-LAST

This is intended to loaded as the final file in layer 2.  

||#

(defun init ()
  (init-xcb)       
  (init-pens)
  (init-fonts)
  (init-winreg)
  (init-event-subsystem)
  )
