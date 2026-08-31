(* TEST
 modules = "stubs.c";
 flambda2;
 {
  ocamlopt_flags = "-extension simd_beta -cc '${cc} -mavx' -ccopt '${cflags}'";
  arch_amd64;
  native;
 }
 {
  arch_arm64;
  native;
 }
*)

(* Regression test: the dummy block for a recursive mixed block with a vector
   field should allocate enough size *)

external interleave_low_64 : int64x2# -> int64x2# -> int64x2#
  = "caml_vec128_unreachable" "caml_simd_vec128_interleave_low_64"
  [@@noalloc] [@@unboxed] [@@builtin]

external interleave_high_64 : int64x2# -> int64x2# -> int64x2#
  = "caml_vec128_unreachable" "caml_simd_vec128_interleave_high_64"
  [@@noalloc] [@@unboxed] [@@builtin]

external low_of : int64 -> int64x2#
  = "caml_vec128_unreachable" "caml_int64x2_low_of_int64"
  [@@noalloc] [@@unboxed] [@@builtin]

external low_to : int64x2# -> int64
  = "caml_vec128_unreachable" "caml_int64x2_low_to_int64"
  [@@noalloc] [@@unboxed] [@@builtin]

let of_i64s x y = interleave_low_64 (low_of x) (low_of y)

let high_to x = low_to (interleave_high_64 x x)

let equal a b =
  Int64.equal (low_to a) (low_to b) && Int64.equal (high_to a) (high_to b)

type t = { t : t option; v : int64x2# }

let expected = of_i64s 0x1234L 0x5678L

let rec t = { t = (Gc.full_major (); Some t); v = expected }

let () =
  Gc.full_major ();
  (match t.t with
   | Some inner -> assert (inner == t)
   | None -> assert false);
  (* An undersized dummy loses the vector's tail when the block is moved. *)
  assert (equal t.v expected)
