(lang dune 3.17)

(name systemp)

(version 1.0)

(generate_opam_files true)

(source
 (github hoffjan/systemp))

(authors "Jan Hoffmann" "Feras Saad")

(maintainers "Feras Saad" "Jan Hoffmann")

(license BSD-3-Clause)

(package
 (name systemp)
 (synopsis "System P")
 (description
  "An implementation of a probabilistic programming language that extends Goedel's System T.")
 (depends ocaml dune core core_unix mparser)
 (tags
  ("System T" "probabiliistc programming" "programming language")))
