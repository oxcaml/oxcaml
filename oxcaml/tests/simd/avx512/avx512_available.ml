(* Exits 0 if the host CPU supports the AVX512 features used by these tests (F,
   VL, DQ, BW), 1 otherwise. The run rules in [dune.inc] use this to skip the
   tests on hosts without AVX512. *)

external available : unit -> bool = "test_avx512_available"

let () = exit (if available () then 0 else 1)
