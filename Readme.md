# CL-XCB - WIP

This is an experimental, minimal set of CFFI bindings for XCB.  Combined with Freetype2, just enough to output decent antialiased text for texty applications.

## About XCB

The underlying library, [XCB](https://xcb.freedesktop.org/), is a replacement for XLIB featuring a much smaller footprint, as well as greatly improved threading support, latency and extensibility.

## Requirements

Obviously your system must have XWindows/XCB libraries installed.  We use the following:

libxcb.so
libxcb-util.so
libxcb-render.so
libxcb-render-util.so
libxcb-icccm.so


Freetype2 is used for glyph rendering.  Many Linux distributions come with these pre-installed, and installing something like the GIMP will probably do the trick.

## Quick Start

(ql:quickload :cl-xcb)
(in-package :xcb)
(init)
(test2)
;; click on go-away and then
(events-process)


## STATUS 
Just working...

## Notes:

### Hacking

### Debug information

For SBCL users, a shell script clean.sh is provided for wiping all fasl files and cache.


Set `*xbug*` t and recompile for debug output.
Set `*compile-checked*` to t and recompile for checked XCB calls.


