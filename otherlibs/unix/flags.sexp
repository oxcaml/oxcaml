; Compiler flags of the library's modules, for its dune file and for the
; build of the library for ocamlopt -target js_of_ocaml
; (external/ast-dependent-libs/stdlib-jsir).
(-absname -g -safe-string -strict-sequence -strict-formats
 -nolabels ; for UnixLabels
 )
