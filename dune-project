(lang dune 2.0)

(name pedro)

(generate_opam_files false)

(source
 (github fferreira/pedro))

(license GPL-3.0-or-later)

(authors "Francisco Ferreira")

(maintainers "Francisco Ferreira")

(homepage "none yet")

(package
 (name pedro)
 (synopsis
   "Some Petri net code")
 (depends mehir ocamlgraph (nuscr (>= 4.4.4)) calistomitososo)
 (description
   "Some Petri net programs...")
)

(using menhir 2.0)
