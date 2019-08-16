## Layer2

Layer2 provides support for a windowing interface via XCB bindings and Layer1.

### Walkthrough

**Layer2-first** has some global variables and initial XCB setup code

**fonts** provides a simple font loader (kind of static for now)

**win-registry** is a global registry for windows.  It maps XCB window ids to Lisp objects.

**geo** is basically a rectangle

**xwin-util** contains a bunch of useful functions that operate on windows

**event-subsystem** builds on Layer1's **event-handling** to provide a generic protocol for implementing event handlers

**Layer2-last** has some global initialization code

