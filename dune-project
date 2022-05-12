(lang dune 2.8)

(name xobl)

(version 0.1)

(license "ISC")

(generate_opam_files true)

(source
 (github steinuil/xobl))

(package
 (name xobl)
 (synopsis "X11 OCaml Bindings Library")
 (description "Pure OCaml implementation of the X11 protocol")
 (depends xmlm ppx_inline_test ppx_expect ppx_deriving))
