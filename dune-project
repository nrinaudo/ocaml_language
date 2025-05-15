(lang dune 3.18)

(name ocaml_language)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Nicolas Rinaudo <nicolas@nrinaudo.com>")

(maintainers "Nicolas Rinaudo <nicolas@nrinaudo.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name ocaml_language)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
