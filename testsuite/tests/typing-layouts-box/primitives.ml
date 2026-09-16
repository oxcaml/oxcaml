(* TEST
 flags = "-extension layouts_beta -extension simd_beta";
 flambda2;
 expect.opt;
*)

(* [%box] is currently only implemented for native code, so this file is run
   with the native toplevel only. *)

external box : ('a : any). ('a[@local_opt]) -> ('a box[@local_opt]) = "%box" [@@layout_poly]
external unbox : ('a : any). ('a box[@local_opt]) -> ('a[@local_opt]) = "%unbox" [@@layout_poly]
[%%expect{|
external box : ('a : any). ('a [@local_opt]) -> ('a box [@local_opt])
  = "%box" [@@layout_poly]
external unbox : ('a : any). ('a box [@local_opt]) -> ('a [@local_opt])
  = "%unbox" [@@layout_poly]
|}]

(* The invariant under test: boxing a value of type [t] produces a block with
   the same layout as a record with a single field of type [t]. When [t] is an
   unboxed record, the block has the same layout as the boxed record.

   Polymorphic equality raises on mixed blocks, so we compare the blocks
   through [Obj]. A failing check raises [Assert_failure], which the expect
   tool records. *)

(* Compare tag, size and scannable prefix. Enough for fields narrower than a
   word, whose padding bits are not initialised. *)
let same_shape (a : Obj.t) (b : Obj.t) =
  Obj.tag a = Obj.tag b
  && Obj.size a = Obj.size b
  && Obj.Uniform_or_mixed.(repr (of_block a) = repr (of_block b))

(* Additionally compare every word of the payload. *)
let same_words (a : Obj.t) (b : Obj.t) =
  same_shape a b
  && List.for_all
       (fun i -> Nativeint.equal (Obj.raw_field a i) (Obj.raw_field b i))
       (List.init (Obj.size a) Fun.id)
[%%expect{|
val same_shape : Obj.t -> Obj.t -> bool = <fun>
val same_words : Obj.t -> Obj.t -> bool = <fun>
|}]

external untag_int : int -> int# = "%int#_of_int"
external int_equal : int# -> int# -> bool = "%int#_equal"
external int8_equal : int8# -> int8# -> bool = "%int8#_equal"
external int16_equal : int16# -> int16# -> bool = "%int16#_equal"
external box_float : float# -> float = "%box_float"
external box_float32 : float32_u -> float32 = "%box_float32"
external box_int32 : int32_u -> int32 = "%box_int32"
external box_int64 : int64_u -> int64 = "%box_int64"
external box_nativeint : nativeint_u -> nativeint = "%box_nativeint"
external box_vec128 : int64x2# -> int64x2 = "%box_vec128"
external unbox_vec128 : int64x2 -> int64x2# = "%unbox_vec128"
[%%expect{|
external untag_int : int -> int# = "%int#_of_int"
external int_equal : int# -> int# -> bool = "%int#_equal"
external int8_equal : int8# -> int8# -> bool = "%int8#_equal"
external int16_equal : int16# -> int16# -> bool = "%int16#_equal"
external box_float : float# -> float = "%box_float"
external box_float32 : float32_u -> float32 = "%box_float32"
external box_int32 : int32_u -> int32 = "%box_int32"
external box_int64 : int64_u -> int64 = "%box_int64"
external box_nativeint : nativeint_u -> nativeint = "%box_nativeint"
external box_vec128 : int64x2# -> int64x2 = "%box_vec128"
external unbox_vec128 : int64x2 -> int64x2# = "%unbox_vec128"
|}]

(* A string allocated at runtime so that the two blocks under comparison share
   a pointer rather than a statically allocated constant. *)
let s = String.make 3 'a'
[%%expect{|
val s : string = "aaa"
|}]

(* Values *)

type 'a vrec = { v : 'a }

let () =
  assert (same_words (Obj.repr (box 42)) (Obj.repr { v = 42 }));
  assert (same_words (Obj.repr (box s)) (Obj.repr { v = s }));
  assert (same_words (Obj.repr (box 3.25)) (Obj.repr { v = 3.25 }));
  assert (same_words (Obj.repr (box (Some s))) (Obj.repr { v = Some s }));
  assert ((Obj.obj (Obj.repr (box 42)) : int vrec).v = 42);
  assert ((Obj.obj (Obj.repr (box s)) : string vrec).v == s)
[%%expect{|
type 'a vrec = { v : 'a; }
|}]

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
  assert (int_equal (Obj.obj (Obj.repr (box (untag_int 42))) : irec).i (untag_int 42))
[%%expect{|
type f64rec = { f64 : float#; }
type i64rec = { i64 : int64_u; }
type nrec = { n : nativeint_u; }
type irec = { i : int#; }
|}]

(* Unboxed numbers narrower than a word *)

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
  assert (int16_equal (Obj.obj (Obj.repr (box #42S)) : i16rec).i16 #42S)
[%%expect{|
type f32rec = { f32 : float32_u; }
type i32rec = { i32 : int32_u; }
type i8rec = { i8 : int8#; }
type i16rec = { i16 : int16#; }
|}]

(* Vectors. A boxed vector is read with an unaligned 16-byte load from the
   block, so we can build one from any two-word block. The words are odd so
   that they look like immediates to the GC. *)

type v128rec = { v128 : int64x2# }

let make_int64x2 (lo : nativeint) (hi : nativeint) : int64x2 =
  let b = Obj.new_block 0 2 in
  Obj.set_raw_field b 0 lo;
  Obj.set_raw_field b 1 hi;
  Obj.obj b

let () =
  let v = unbox_vec128 (make_int64x2 43n 45n) in
  assert (same_words (Obj.repr (box v)) (Obj.repr { v128 = v }));
  let r : v128rec = Obj.obj (Obj.repr (box v)) in
  let b = Obj.repr (box_vec128 r.v128) in
  assert (Nativeint.equal (Obj.raw_field b 0) 43n);
  assert (Nativeint.equal (Obj.raw_field b 1) 45n)
[%%expect{|
type v128rec = { v128 : int64x2#; }
val make_int64x2 : nativeint -> nativeint -> int64x2 = <fun>
|}]

(* Unboxed products. Boxing a product should produce the same block as a
   record whose fields are the components, including the reordering of values
   before flat fields, and flattening of nested products. *)

type p_values = { a : int; b : string }
type p_flat_first = { c : int64_u; d : string }
type p_value_first = { e : string; f : int64_u }
type p_many = { g : int64_u; h : float#; k : string; l : int; m : int64_u }
type p_nested = { n1 : #(int64_u * string); n2 : float# }
type p_flattened = { q1 : int64_u; q2 : string; q3 : float# }

let () =
  assert (same_words (Obj.repr (box #(42, s))) (Obj.repr { a = 42; b = s }))
[%%expect{|
type p_values = { a : int; b : string; }
type p_flat_first = { c : int64_u; d : string; }
type p_value_first = { e : string; f : int64_u; }
type p_many = { g : int64_u; h : float#; k : string; l : int; m : int64_u; }
type p_nested = { n1 : #(int64_u * string); n2 : float#; }
type p_flattened = { q1 : int64_u; q2 : string; q3 : float#; }
|}]

(* CR zeisbach: [#(int64_u * string) box] expands to the tuple type
   [int64_u * string], and [Typeopt.value_kind] recurses into the components
   of a tuple type and rejects the non-value ones. Until that is fixed, boxing
   a product with unboxed components through [box] fails to compile, so the
   tests below are disabled and unboxed records are used instead.

let _ = box #(#42L, s)
[%%expect{|
Line 1, characters 8-22:
1 | let _ = box #(#42L, s)
            ^^^^^^^^^^^^^^
Error: Non-value detected in [value_kind].
       Please report this error to the Jane Street compilers team.
       The layout of int64_u is bits64
         because it is the primitive type int64_u.
       But the layout of int64_u must be a value layout
         because it has to be value for the V1 safety check.
|}]

external box_obj : ('a : any). 'a -> Obj.t = "%box" [@@layout_poly]
[%%expect{|
external box_obj : ('a : any). 'a -> Obj.t = "%box" [@@layout_poly]
|}]

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
[%%expect{||}]
*)

(* Boxing an unboxed record [t#] produces a [t] with the same layout as
   constructing [t] directly, including the reordering of values before flat
   fields and the flattening of nested products. *)

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
  assert (Float.equal (box_float nested.n2) 2.5)
[%%expect{|
|}]

(* Boxing an unboxed record [t#] produces a [t]. *)

type ur = { u1 : int64_u; u2 : string; u3 : int }

let () =
  let boxed : ur = box #{ u1 = #42L; u2 = s; u3 = 7 } in
  assert (same_words (Obj.repr boxed) (Obj.repr { u1 = #42L; u2 = s; u3 = 7 }));
  assert (Int64.equal (box_int64 boxed.u1) 42L && boxed.u2 == s && boxed.u3 = 7)
[%%expect{|
type ur = { u1 : int64_u; u2 : string; u3 : int; }
|}]
