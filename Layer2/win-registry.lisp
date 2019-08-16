(in-package :xcb)

;;================================================================================
#|| WIN-REGISTRY

XCB windows are assigned a unique ID at creation time.  This ID is used by XCB 
for all operations on that window.  Most importantly, events report the ID
which must be converted back to a Lisp object.

A simple (but perhaps not most efficient) strategy is to use a hash-table to
resolve IDs to list window objects.  Hash-tables are probably an overkill
unless we have tens of windows to deal with, but for now...

||#

(defparameter *winreg* nil)

(defun init-winreg ()
  (setf *winreg* (make-hash-table)))

(defun winreg-register (id object)
  "Associate ID with object"
  (setf (gethash id *winreg*) object) )

(defun winreg-unregister (id)
  "Remove id and its association"
  (remhash id *winreg*))

(defun winreg-ref (id)
  "Get the object at id"
  (gethash id *winreg*))

