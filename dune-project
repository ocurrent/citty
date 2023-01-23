(lang dune 3.6)
(name citty)

(generate_opam_files true)

(source (github ocurrent/citty))
(authors "Frédéric Bour")
(maintainers "Frédéric Bour")
(license "MIT")

(package
 (name citty)
 (synopsis "CI in TTY")
 (depends
  current_rpc
  capnp
  capnp-rpc-unix
  (lwt (>= 5.6.1))
  (notty (>= 0.2.3))
  (lwd (>= 0.3))
  (nottui (>= 0.3))
  (nottui-lwt (>= 0.3))
  ocaml-ci-api
  fpath
  fmt
  logs
  (cmdliner (>= 1.1.0))))
