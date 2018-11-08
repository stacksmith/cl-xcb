(in-package :xcb)

(cffi:define-foreign-library libxcb
    (:unix "libxcb.so"))
(cffi:use-foreign-library libxcb)

(cffi:define-foreign-library libxcb-util
    (:unix "libxcb-util.so"))
(cffi:use-foreign-library libxcb-util)

(cffi:define-foreign-library libxcb-render
    (:unix "libxcb-render.so"))
(cffi:use-foreign-library libxcb-render)

(cffi:define-foreign-library libxcb-render-util
  (:unix (:or "libxcb-render-util.so" "libxcb-render-util.so.0")))
(cffi:use-foreign-library libxcb-render-util)

(cffi:define-foreign-library libxcb-icccm
  (:unix (:or "libxcb-icccm.so" "libxcb-icccm.so.4")))
(cffi:use-foreign-library libxcb-icccm)
