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

1) _Organisation and repositories_.  mirage/irmin is hardcoded for now.  We
need interfaces, programmatic and end-user ones, to list and select
organisations and repositories.

2) _Moving around with keyboard_.  Scrolling the log can be done with mouse for
now.  Next: bind page-up, page-down, home, end, ...

3) _Search and visualize log_.  Add searching ('/') and highlighting, detect
some patterns (e.g. regular-expressions).

4) _Shell out_.  The most basic interaction will be to send the log to a pager
or editor.  Eventually, running custom shell commands might be useful too. 
