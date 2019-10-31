(lang dune 1.10)
(name citty)
(using fmt 1.2)

(generate_opam_files true)
(source (github ocaml-ci/citty))
(authors "Frédéric Bour")

(package
 (name citty)
 (synopsis "CI in TTY")
 (depends current_rpc capnp capnp-rpc-unix lwt notty fpath fmt))
