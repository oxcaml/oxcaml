; Flags for compiling the DWARF test executables, spliced into each
; executable stanza's ocamlopt_flags via (:include ...). The optimization
; level is pinned to -O3 so the DWARF output is stable regardless of the
; dune build profile. -gdwarf-may-alter-codegen asks the optimizer not to
; merge instructions from distinct source locations, so that stepping
; through the source line by line (e.g. in test_stepping_dwarf) remains
; possible at -O3.
(-g -gno-upstream-dwarf -bin-annot-cms -gdwarf-fidelity high
 -shape-format debugging-shapes -extension simd_beta -gdwarf-pedantic
 -function-sections -O3 -gdwarf-may-alter-codegen)
