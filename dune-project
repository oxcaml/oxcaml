(lang dune 3.19)
(wrapped_executables false)
(using experimental_building_ocaml_compiler_with_dune 0.1)
(using menhir 2.1)

(use_standard_c_and_cxx_flags true)
(cram enable)

(authors "OCaml Developers" "Jane Street Group, LLC")
(maintainers "Jane Street developers")
(source (github oxcaml/oxcaml))
(homepage "https://oxcaml.org/")
(documentation "https://oxcaml.org/documentation/")
(license "GPL-2.0-or-later" "LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception" "MIT")

(package
  (name oxcaml)
  (synopsis "A fast-moving set of extensions to the OCaml programming language")
  (depends
    (ocaml (and (>= 4.14) (< 5.0)))
    (menhir (= 20231231))
    (ocamlformat (= 0.24.1)))
)

(package
  (name ocaml)
)

(package
  (name ocaml_runtime_stdlib)
)
