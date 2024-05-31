(lang dune 3.15)

(using menhir 3.0 )

(name mlisp)

; (source
;  (github username/reponame))

(authors "Cherry Sigma")

(maintainers "Cherry Sigma")

(license Open Domain)

; (documentation https://url/to/documentation)

(package
 (name mlisp)
 (synopsis "mlisp")
 (description "minimal lisp")
 (depends ocaml dune menhirLib))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
