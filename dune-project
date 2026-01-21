(lang dune 3.20)

(using menhir 3.0)

(name ltl-weaken)

(generate_opam_files true)

(source
 (github benmandrew/ltl-weaken))

(authors "Ben M. Andrew <benmandrew@gmail.com>")

(maintainers "Ben M. Andrew <benmandrew@gmail.com>")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name ltl-weaken)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  (why3 (>= 1.8.0))
  (core (>= 0.17.0))
  (core_unix (>= 0.17.0))
  xml-light
  cmdliner)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
