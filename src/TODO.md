Code
====

1) _Resource management_.  Capnp references are never released, we need a
pattern to tie a resource to an UI element so that their lifetime can be
managed together.

2) _Cleanup_.  The "widgets" are completely ad-hoc, state and UI logic are
interleaved.  It makes sense while experimenting with UI concepts, but that
should be cleaned up rather soon.  

UI
==

3) _Search and visualize log_.  Add searching ('/') and highlighting, detect
some patterns (e.g. regular-expressions).
