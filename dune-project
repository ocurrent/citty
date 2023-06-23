(lang dune 2.0)

(name citty)

(generate_opam_files true)
(source (github ocaml-ci/citty))
(authors "Frédéric Bour")
(maintainers "Navin Keswani <navin@novemberkilo.io>" "Tim McGilchrist <timmcgil@gmail.com>")
(license "MIT")

(package
 (name citty)
 (synopsis "CI in TTY")
 (depends current_rpc capnp capnp-rpc-unix lwt notty fpath fmt))
