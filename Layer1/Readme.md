## Layer1

Building on BINDINGS, this layer adds some useful functionality.



### ft2.lisp

Extends FreeType2 bindings to provide glyph rendering services for our XCB framework.

* GLYPH-TRIE datastructure for caching groups of rendered glyphs.

* Extraction of FT2 glyphs into XCB glyphs

### keys.lisp

Provides enumeration and lookup for keys in key events

### event-handling.lisp

* a dispatch table for events;
* a dispatch mechanism;
* An event stepper
* a blocking event loop with a thread-proc
* events-process function for use in a single thread

### pens.lisp

A pen is a struct containing a 1x1 pixmap containing a single pixel of a given color, along with a 64-bit ABGR representation of the same color.  It is useful for both xrender operations and normal operations.

