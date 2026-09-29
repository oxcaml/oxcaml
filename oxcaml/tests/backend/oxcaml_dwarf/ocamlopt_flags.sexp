; Flags for compiling the DWARF test executables, spliced into each
; executable stanza's ocamlopt_flags via (:include ...). The optimization
; level is pinned to -O3 so the DWARF output is stable regardless of the
; dune build profile.
(-g -gno-upstream-dwarf -bin-annot-cms -gdwarf-fidelity high
 -shape-format debugging-shapes -extension simd_beta -gdwarf-pedantic
 -function-sections -O3)
