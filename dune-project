(lang dune 2.0)

(name citty)

(generate_opam_files true)
(source (github ocurrent/citty))
(authors "Frédéric Bour")
(maintainers "Frédéric Bour")

(package
 (name citty)
 (synopsis "CI in TTY")
 (depends current_rpc capnp capnp-rpc-unix lwt notty fpath fmt))
