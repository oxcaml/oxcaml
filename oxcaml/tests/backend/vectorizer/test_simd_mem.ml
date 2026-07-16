[@@@ocaml.warnerror "+a-40-41-42"]

(* Regression test for a compile-time crash: instruction selection produces [Isimd_mem]
   for the SIMD load/store intrinsics, and the vectorizer's dependency analysis used to
   fail with a fatal error on such instructions (it incorrectly assumed they could not
   occur before vectorization). The two constant stores form a candidate seed, which
   forces the dependency analysis to classify every instruction of the block, including
   the SIMD load. The SIMD load is never executed at run time; this test only checks that
   the function compiles with [-vectorize]. *)

type addr = nativeint#

external unbox_nativeint : nativeint -> addr = "%unbox_nativeint" [@@warning "-187"]

external vec128_load_unaligned
  :  addr
  -> (int64x2[@unboxed])
  = "" "caml_sse_vec128_load_unaligned"
[@@noalloc] [@@builtin]

type t2 =
  { mutable d0 : int
  ; mutable d1 : int
  }

let[@opaque] store_pair_and_load (r : t2) (p : nativeint) : int64x2 =
  r.d0 <- 1;
  r.d1 <- 2;
  vec128_load_unaligned (unbox_nativeint p)
;;

let () =
  let r = { d0 = 0; d1 = 0 } in
  if Sys.opaque_identity false
  then ignore (Sys.opaque_identity (store_pair_and_load r 0n) : int64x2);
  Format.printf "ok { d0 = %d ; d1 = %d }\n" r.d0 r.d1
;;
