(* TEST
 modules = "stubs.c";
 flags = "-extension layouts_beta -extension simd_beta";
 flambda2;
 native;
*)

(* [%box] is currently only implemented for native code. *)

external box : ('a : any). ('a[@local_opt]) -> ('a box[@local_opt]) = "%box" [@@layout_poly]
external unbox : ('a : any). ('a box[@local_opt]) -> ('a[@local_opt]) = "%unbox" [@@layout_poly]

(* TESTING INVARIANT: boxing a value of type [t] produces a block with the same
   layout as a record with a single field of type [t]. When [t] is an unboxed
   record, the block has the same layout as the boxed record. *)

(* comparison using [Obj] to avoid (busted) polymorphic compare *)
let same_shape (a : Obj.t) (b : Obj.t) =
  Obj.tag a = Obj.tag b
  && Obj.size a = Obj.size b
  && Obj.Uniform_or_mixed.(repr (of_block a) = repr (of_block b))

let same_words (a : Obj.t) (b : Obj.t) =
  same_shape a b
  && List.for_all
       (fun i -> Nativeint.equal (Obj.raw_field a i) (Obj.raw_field b i))
       (List.init (Obj.size a) Fun.id)

(* helpers for equality *)
external untag_int : int -> int# = "%int#_of_int"
external int_equal : int# -> int# -> bool = "%int#_equal"
external int8_equal : int8# -> int8# -> bool = "%int8#_equal"
external int16_equal : int16# -> int16# -> bool = "%int16#_equal"
external box_float : float# -> float = "%box_float"
external box_float32 : float32_u -> float32 = "%box_float32"
external box_int32 : int32_u -> int32 = "%box_int32"
external box_int64 : int64_u -> int64 = "%box_int64"
external box_nativeint : nativeint_u -> nativeint = "%box_nativeint"

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

(* A string allocated at runtime so that the two blocks under comparison share
   a pointer rather than a statically allocated constant. *)
let s = String.make 3 'a'

(* Values *)

type 'a vrec = { v : 'a }

type mixed = { mx : int64_u; my : string }
type with_product = { wp : #(int64_u * string); wq : int }
type all_flat = { af : float#; ag : int32_u }
type float_record = { fr1 : float; fr2 : float }
type variant = A of int64_u * string | B | C of #(float# * int)

let () =
  assert (same_words (Obj.repr (box 42)) (Obj.repr { v = 42 }));
  assert (same_words (Obj.repr (box s)) (Obj.repr { v = s }));
  assert (same_words (Obj.repr (box 3.25)) (Obj.repr { v = 3.25 }));
  assert (same_words (Obj.repr (box (Some s))) (Obj.repr { v = Some s }));
  assert ((Obj.obj (Obj.repr (box 42)) : int vrec).v = 42);
  assert ((Obj.obj (Obj.repr (box s)) : string vrec).v == s);
  let m = { mx = #1L; my = s } in
  assert (same_words (Obj.repr (box m)) (Obj.repr { v = m }));
  assert ((Obj.obj (Obj.repr (box m)) : mixed vrec).v == m);
  let p = { wp = #(#2L, s); wq = 3 } in
  assert (same_words (Obj.repr (box p)) (Obj.repr { v = p }));
  assert ((Obj.obj (Obj.repr (box p)) : with_product vrec).v == p);
  let f = { af = #1.5; ag = #4l } in
  assert (same_words (Obj.repr (box f)) (Obj.repr { v = f }));
  assert ((Obj.obj (Obj.repr (box f)) : all_flat vrec).v == f);
  let fr = { fr1 = 1.5; fr2 = 2.5 } in
  assert (same_words (Obj.repr (box fr)) (Obj.repr { v = fr }));
  assert ((Obj.obj (Obj.repr (box fr)) : float_record vrec).v == fr);
  let a = A (#5L, s) and b = B and c = C #(#6.5, 7) in
  assert (same_words (Obj.repr (box a)) (Obj.repr { v = a }));
  assert (same_words (Obj.repr (box b)) (Obj.repr { v = b }));
  assert (same_words (Obj.repr (box c)) (Obj.repr { v = c }));
  assert ((Obj.obj (Obj.repr (box a)) : variant vrec).v == a);
  assert ((Obj.obj (Obj.repr (box c)) : variant vrec).v == c);
  print_endline "values: ok"

(* Unboxed numbers stored in a whole word *)

type f64rec = { f64 : float# }
type i64rec = { i64 : int64_u }
type nrec = { n : nativeint_u }
type irec = { i : int# }

let () =
  assert (same_words (Obj.repr (box #3.25)) (Obj.repr { f64 = #3.25 }));
  assert (same_words (Obj.repr (box #42L)) (Obj.repr { i64 = #42L }));
  assert (same_words (Obj.repr (box #42n)) (Obj.repr { n = #42n }));
  assert (same_words (Obj.repr (box (untag_int 42))) (Obj.repr { i = untag_int 42 }));
  assert (Float.equal (box_float (Obj.obj (Obj.repr (box #3.25)) : f64rec).f64) 3.25);
  assert (Int64.equal (box_int64 (Obj.obj (Obj.repr (box #42L)) : i64rec).i64) 42L);
  assert (Nativeint.equal (box_nativeint (Obj.obj (Obj.repr (box #42n)) : nrec).n) 42n);
  assert (int_equal (Obj.obj (Obj.repr (box (untag_int 42))) : irec).i (untag_int 42));
  print_endline "whole-word numbers: ok"

(* Unboxed numbers narrower than a word. for now, these are treated as
   addressable and boxed as tag-0 blocks, NOT as tagged immediates. eventually,
   addressable kinds will allow expression of both behaviors. *)

type f32rec = { f32 : float32_u }
type i32rec = { i32 : int32_u }
type i8rec = { i8 : int8# }
type i16rec = { i16 : int16# }

let () =
  assert (same_shape (Obj.repr (box #3.25s)) (Obj.repr { f32 = #3.25s }));
  assert (same_shape (Obj.repr (box #42l)) (Obj.repr { i32 = #42l }));
  assert (same_shape (Obj.repr (box #42s)) (Obj.repr { i8 = #42s }));
  assert (same_shape (Obj.repr (box #42S)) (Obj.repr { i16 = #42S }));
  assert (box_float32 (Obj.obj (Obj.repr (box #3.25s)) : f32rec).f32
          = box_float32 #3.25s);
  assert (Int32.equal (box_int32 (Obj.obj (Obj.repr (box #42l)) : i32rec).i32) 42l);
  assert (int8_equal (Obj.obj (Obj.repr (box #42s)) : i8rec).i8 #42s);
  assert (int16_equal (Obj.obj (Obj.repr (box #42S)) : i16rec).i16 #42S);
  print_endline "sub-word numbers: ok"

(* CR zeisbach: consider testing these... *)
(* Vectors. 512-bit vectors and masks need AVX-512 at compile and run time,
   which ocamltest cannot gate on, so they are not tested currently. *)

type v128rec = { v128 : int64x2# }
type v256rec = { v256 : int64x4# }

let () =
  let v = int64x2 43L 45L in
  assert (same_words (Obj.repr (box v)) (Obj.repr { v128 = v }));
  let r : v128rec = Obj.obj (Obj.repr (box v)) in
  assert (Int64.equal (low r.v128) 43L);
  assert (Int64.equal (high r.v128) 45L);
  print_endline "vec128: ok"

let () =
  let v = join_vec256 (int64x2 1L 2L) (int64x2 3L 4L) in
  assert (same_words (Obj.repr (box v)) (Obj.repr { v256 = v }));
  let r : v256rec = Obj.obj (Obj.repr (box v)) in
  let #(l, h) = split_vec256 r.v256 in
  assert (Int64.equal (low l) 1L && Int64.equal (high l) 2L);
  assert (Int64.equal (low h) 3L && Int64.equal (high h) 4L);
  print_endline "vec256: ok"

(* Unboxed products *)

type p_values = { a : int; b : string }
type p_flat_first = { c : int64_u; d : string }
type p_value_first = { e : string; f : int64_u }
type p_many = { g : int64_u; h : float#; k : string; l : int; m : int64_u }
type p_nested = { n1 : #(int64_u * string); n2 : float# }
type p_flattened = { q1 : int64_u; q2 : string; q3 : float# }

let () =
  assert (same_words (Obj.repr (box #(42, s))) (Obj.repr { a = 42; b = s }));
  print_endline "products: ok"

(* CR zeisbach: add tests for mixed tuples after rebasing onto them *)

(*
external box_obj : ('a : any). 'a -> Obj.t = "%box" [@@layout_poly]

let () =
  assert (same_words (box_obj #(#42L, s)) (Obj.repr { c = #42L; d = s }));
  assert (same_words (box_obj #(s, #42L)) (Obj.repr { e = s; f = #42L }));
  assert (same_words (box_obj #(#1L, #2.5, s, 3, #4L))
            (Obj.repr { g = #1L; h = #2.5; k = s; l = 3; m = #4L }));
  assert (same_words (box_obj #(#(#42L, s), #2.5))
            (Obj.repr { n1 = #(#42L, s); n2 = #2.5 }));
  assert (same_words (box_obj #(#(#42L, s), #2.5))
            (Obj.repr { q1 = #42L; q2 = s; q3 = #2.5 }));
  let r : p_flat_first = Obj.obj (box_obj #(#42L, s)) in
  assert (Int64.equal (box_int64 r.c) 42L && r.d == s);
  let r : p_many = Obj.obj (box_obj #(#1L, #2.5, s, 3, #4L)) in
  assert (Int64.equal (box_int64 r.g) 1L);
  assert (Float.equal (box_float r.h) 2.5);
  assert (r.k == s && r.l = 3);
  assert (Int64.equal (box_int64 r.m) 4L)
*)


type ur = { u1 : int64_u; u2 : string; u3 : int }

let () =
  let flat_first : p_flat_first = box #{ c = #42L; d = s } in
  assert (same_words (Obj.repr flat_first) (Obj.repr { c = #42L; d = s }));
  assert (Int64.equal (box_int64 flat_first.c) 42L && flat_first.d == s);
  let value_first : p_value_first = box #{ e = s; f = #42L } in
  assert (same_words (Obj.repr value_first) (Obj.repr { e = s; f = #42L }));
  assert (value_first.e == s && Int64.equal (box_int64 value_first.f) 42L);
  let many : p_many = box #{ g = #1L; h = #2.5; k = s; l = 3; m = #4L } in
  assert (same_words (Obj.repr many)
            (Obj.repr { g = #1L; h = #2.5; k = s; l = 3; m = #4L }));
  assert (Int64.equal (box_int64 many.g) 1L);
  assert (Float.equal (box_float many.h) 2.5);
  assert (many.k == s && many.l = 3);
  assert (Int64.equal (box_int64 many.m) 4L);
  let nested : p_nested = box #{ n1 = #(#42L, s); n2 = #2.5 } in
  assert (same_words (Obj.repr nested) (Obj.repr { n1 = #(#42L, s); n2 = #2.5 }));
  let #(n1a, n1b) = nested.n1 in
  assert (Int64.equal (box_int64 n1a) 42L && n1b == s);
  assert (Float.equal (box_float nested.n2) 2.5);
  let boxed : ur = box #{ u1 = #42L; u2 = s; u3 = 7 } in
  assert (same_words (Obj.repr boxed) (Obj.repr { u1 = #42L; u2 = s; u3 = 7 }));
  assert (Int64.equal (box_int64 boxed.u1) 42L && boxed.u2 == s && boxed.u3 = 7);
  print_endline "unboxed records: ok"
