(lang dune 2.9)
(name bozon)
(using coq 0.3)

(generate_opam_files true)

(license MIT)

(maintainers "remimimimi<valent.xarin@gmail.com>")
(authors "remimimimi<valent.xarin@gmail.com>")

(source (github remimimimi/lambda-calculus-typecheckers))
(homepage "https://github.com/remimimimi/Bozon")
(package
 (name bozon)
 (synopsis "Bozon is a new programming language designed to bring together the best features of the worlds")
 (description "Bozon is a new programming language designed to bring together the best features of the worlds")
 (depends
  (ocaml (>= 4.11.1))
  (coq (>= 8.14.0))
  (coq-hammer (>= 1.3.2))
  (coq-hammer-tactics (>= 1.3.2))))
