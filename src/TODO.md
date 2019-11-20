UI framework
============

1) _Resource management_.  Capnp references are never released, we need a
pattern to tie a resource to an UI element so that their lifetime can be
managed together.

... That's annoying, I feel like the Capnp interface is not helping much.

2) Customizing UI scheduling. The UI is composed in two passes:
- layout is computed from leafs to root (building the Nottui values)
- rendered image is composed from root to leafs

These passes are informal, but for implementing more difficult layout and
drawing schemes it might make sense to make these passes explicit and allow
"hooking" into them (for instance, the update content after layout and before
rendering).
Such as text layout which depends on the "box layout" having been computed
already or "virtual" widgets which determine their contents based on the
visible area.

3) Lwd: to get a finer grained invalidation model, maybe a node to trace
effects...

Citty
=====

1) _Search and visualize log_.  Add searching ('/') and highlighting, detect
some patterns (e.g. regular-expressions).
