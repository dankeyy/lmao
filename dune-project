(lang dune 3.19)

(name lmao)

(generate_opam_files true)

(source
 (github dankeyy/lmao))

(authors "dankeyy")
(maintainers "dankeyy")

(license LICENSE)


(package
 (name lmao)
 (synopsis "lambda calc playground")
 (description "for getting a better hang at beta reductions etc")
 (depends ocaml)
 (tags
  ("fp" "lambda calculus")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
