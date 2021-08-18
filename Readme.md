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
(make-win 640 480)
;; maybe keep it as a topmost, resize  or  click on go-away and then
(events-process)


## STATUS 
Just working...

## Notes:

### Hacking

There are three distinct layers, each in its own directory:

#### Bindings

Here we have actual CFFI bindings, c-structs and constants for xcb, icccm, xrender, and events.  `libs.lisp' code actually loads the libraries.

#### Layer 1

* FreeType2 functionality in `ft2.lisp`, antialiased glyph conversion and storage in a trie.
* Basic event processing.  
* Mapping of keys to characters.

#### Layer 2

* Runtime setup and initialization of XCB 
* A simple default font loader
* Windows registry
* Simple Geometry (points, rects)
* XWIN-UTIL - simple utilities for windowing
* Event-subsystem - a basic protocol for dispatching events to windows
* Final initialization step

#### CLEAN.SH

For sbcl/emacs users, a shell script clean.sh is provided for wiping all fasl files and cache (.cache/common-lisp/ keeps often-stale fasls!)

#### DEBUG MODES

Set `*xbug*` t and recompile for debug output.
Set `*compile-checked*` to t and recompile for checked XCB calls.

