(lang dune 3.23)
(formatting (enabled_for ocaml))
(wrapped_executables false)
(using experimental_building_ocaml_compiler_with_dune 0.1)
(using menhir 2.1)

(use_standard_c_and_cxx_flags true)
(cram enable)

(package
  (name oxcaml)
)

(package
  (name ocaml)
)

(package
  (name oxcaml-dev)
)

; Public names for libraries that installed compilers provide through findlib,
; for the vendored libraries in external/ast-dependent-libs, which are separate
; dune projects and cannot see this project's private libraries:
; compiler-libs.common is ocamlcommon, and so on, and
; compiler-libs.internal.flambda2_floats is a dependency of ocamlfrontend.
; Nothing installs these packages.
(package
  (name compiler-libs)
)

(package
  (name unix)
)

(package
  (name str)
)

(package
  (name runtime_events)
)
