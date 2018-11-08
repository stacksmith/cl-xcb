(in-package :xcb)

;;=============================================================================
;; DYNARRAY
;;
;; A dynamic array.  Behaves like a normal array of T, except items may be
;; marked as deleted.  A free list (index chain from item 0) allows reuse of
;; slots.
;;
;; Rationale: for tracking slow objects with a lifespan, like window handlers,
;; this is a better solution than a hashtable.
;;
;; TODO: later.
