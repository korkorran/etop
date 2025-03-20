(lang dune 3.16)

(name etop)

(generate_opam_files true)

(source
 (github korkorran/etop))

(authors "Frédéric Lang")

(maintainers "Frédéric Lang")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name etop)
 (synopsis "Command line assitant")
 (description "Etop helps to write commands")
 (depends ocaml dune)
 (tags
  (command terminal assistant)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
