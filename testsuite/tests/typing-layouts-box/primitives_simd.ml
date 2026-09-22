(* TEST
 modules = "stubs.c";
 flags = "-extension layouts_alpha -extension simd_beta";
 flambda2;
 native;
*)

external box : ('a : any). ('a[@local_opt]) -> ('a box[@local_opt]) = "%box" [@@layout_poly]
external unbox : ('a : any). ('a box[@local_opt]) -> ('a[@local_opt]) = "%unbox" [@@layout_poly]

(* Vector kinds are addressable, so boxing matches a record field. *)

(* Only the header is compared; contents are read back through the record
   type, as in [box_primitive.ml]. *)
let same_shape (a : Obj.t) (b : Obj.t) =
  Obj.tag a = Obj.tag b
  && Obj.size a = Obj.size b
  && Obj.Uniform_or_mixed.(repr (of_block a) = repr (of_block b))

(* Vector construction via compiler builtins, as in
   [typing-layouts-arrays/vector_elem.ml]. *)
external int64x2_of_int64 : int64 -> int64x2#
  = "caml_vec128_unreachable" "caml_int64x2_low_of_int64"
  [@@noalloc] [@@unboxed] [@@builtin]
external int64_of_int64x2 : int64x2# -> int64
  = "caml_vec128_unreachable" "caml_int64x2_low_to_int64"
  [@@noalloc] [@@unboxed] [@@builtin]
external interleave_low_64 : int64x2# -> int64x2# -> int64x2#
  = "caml_vec128_unreachable" "caml_simd_vec128_interleave_low_64"
  [@@noalloc] [@@unboxed] [@@builtin]
external interleave_high_64 : int64x2# -> int64x2# -> int64x2#
  = "caml_vec128_unreachable" "caml_simd_vec128_interleave_high_64"
  [@@noalloc] [@@unboxed] [@@builtin]
external join_vec256 : int64x2# -> int64x2# -> int64x4# = "%join_vec256"
external split_vec256 : int64x4# -> #(int64x2# * int64x2#) = "%split_vec256"

let int64x2 lo hi = interleave_low_64 (int64x2_of_int64 lo) (int64x2_of_int64 hi)
let low (v : int64x2#) = int64_of_int64x2 v
let high (v : int64x2#) = int64_of_int64x2 (interleave_high_64 v v)

type v128rec = { v128 : int64x2# }
type v256rec = { v256 : int64x4# }
type inherited_v128 = #{ inherit inherited_v128 : int64x2# }

let () =
  let v = int64x2 43L 45L in
  assert (same_shape (Obj.repr (box v)) (Obj.repr { v128 = v }));
  let r : v128rec = Obj.obj (Obj.repr (box v)) in
  assert (Int64.equal (low r.v128) 43L);
  assert (Int64.equal (high r.v128) 45L);
  let u = unbox (box v) in
  assert (Int64.equal (low u) 43L && Int64.equal (high u) 45L);
  let u = unbox (Sys.opaque_identity (box v)) in
  assert (Int64.equal (low u) 43L && Int64.equal (high u) 45L);
  let inherited = box #{ inherited_v128 = v } in
  assert (same_shape (Obj.repr inherited) (Obj.repr { v128 = v }));
  let #{ inherited_v128 = u } = unbox (Sys.opaque_identity inherited) in
  assert (Int64.equal (low u) 43L && Int64.equal (high u) 45L);
  print_endline "vec128: ok"

let () =
  let v = join_vec256 (int64x2 1L 2L) (int64x2 3L 4L) in
  assert (same_shape (Obj.repr (box v)) (Obj.repr { v256 = v }));
  let r : v256rec = Obj.obj (Obj.repr (box v)) in
  let #(l, h) = split_vec256 r.v256 in
  assert (Int64.equal (low l) 1L && Int64.equal (high l) 2L);
  assert (Int64.equal (low h) 3L && Int64.equal (high h) 4L);
  let #(l, h) = split_vec256 (unbox (Sys.opaque_identity (box v))) in
  assert (Int64.equal (low l) 1L && Int64.equal (high l) 2L);
  assert (Int64.equal (low h) 3L && Int64.equal (high h) 4L);
  print_endline "vec256: ok"

(* Local allocation of a boxed vector. *)

external globalize : local_ 'a -> 'a = "%obj_dup"

let () =
  let local_ vec = box (int64x2 43L 45L) in
  assert (same_shape (Obj.repr (globalize vec)) (Obj.repr { v128 = int64x2 43L 45L }));
  let r : v128rec = Obj.obj (Obj.repr (globalize vec)) in
  assert (Int64.equal (low r.v128) 43L && Int64.equal (high r.v128) 45L);
  print_endline "local vec128: ok"

(* Aliasing. Two heap-allocated boxes of the same vector must be distinct
   blocks.*)

type vrec = { mutable v : int64x2#; tag : int }

let () =
  let x = int64x2 (Sys.opaque_identity 1L) 2L in
  let r1 = (box #{ v = x; tag = 7 } : vrec @ global) in
  let r2 = (box #{ v = x; tag = 7 } : vrec @ global) in
  assert (r1 != r2);
  r1.v <- int64x2 20L 30L;
  assert (Int64.equal (low r2.v) 1L);
  assert (Int64.equal (low r1.v) 20L);
  (* Unboxing reads the mutated vector. *)
  let #{ v; tag } = unbox r1 in
  assert (Int64.equal (low v) 20L && Int64.equal (high v) 30L && tag = 7);
  print_endline "aliasing: ok"
