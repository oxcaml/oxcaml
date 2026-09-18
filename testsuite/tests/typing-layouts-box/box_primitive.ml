(* TEST
 include stdlib_stable;
 flags = "-extension layouts_beta";
 flambda2;
 { expect; expect.opt; }
*)

external box : ('a : any). ('a[@local_opt]) -> ('a box[@local_opt]) = "%box" [@@layout_poly]
[%%expect{|
external box : ('a : any). ('a [@local_opt]) -> ('a box [@local_opt])
  = "%box" [@@layout_poly]
|}]

(* TESTING INVARIANT: boxing a value of type [t] produces a block with the same
   layout as a record with a single field of type [t]. When [t] is an unboxed
   record, the block has the same layout as the boxed record. *)

(* comparison using [Obj] to avoid (busted) polymorphic compare *)

let native () = match Sys.backend_type with Native -> true | _ -> false

let same_shape (a : Obj.t) (b : Obj.t) =
  Obj.tag a = Obj.tag b
  && Obj.size a = Obj.size b
  && Obj.Uniform_or_mixed.(repr (of_block a) = repr (of_block b))

(* [boxed] must have the same shape as [record], the record it is supposed to
   be laid out like. Contents are read back by reinterpreting [boxed] as that
   record type with [Obj.magic]. *)
let check_shape boxed record =
  assert (same_shape (Obj.repr boxed) (Obj.repr record))
[%%expect{|
val native : unit -> bool = <fun>
val same_shape : Obj.t -> Obj.t -> bool = <fun>
val check_shape : 'a -> 'b -> unit = <fun>
|}]

external untag_int : int -> int# = "%int#_of_int"
external eq_int : int# -> int# -> bool = "%int#_equal"
external eq_i8 : int8# -> int8# -> bool = "%int8#_equal"
external eq_i16 : int16# -> int16# -> bool = "%int16#_equal"
external box_float : float# -> float = "%box_float"
external box_float32 : float32_u -> float32 = "%box_float32"
external box_int32 : int32_u -> int32 = "%box_int32"
external box_int64 : int64_u -> int64 = "%box_int64"
external box_nativeint : nativeint_u -> nativeint = "%box_nativeint"

let eq_f64 x y = Float.equal (box_float x) (box_float y)
let eq_f32 x y = box_float32 x = box_float32 y
let eq_i32 x y = Int32.equal (box_int32 x) (box_int32 y)
let eq_i64 x y = Int64.equal (box_int64 x) (box_int64 y)
let eq_n x y = Nativeint.equal (box_nativeint x) (box_nativeint y)
[%%expect{|
external untag_int : int -> int# = "%int#_of_int"
external eq_int : int# -> int# -> bool = "%int#_equal"
external eq_i8 : int8# -> int8# -> bool = "%int8#_equal"
external eq_i16 : int16# -> int16# -> bool = "%int16#_equal"
external box_float : float# -> float = "%box_float"
external box_float32 : float32_u -> float32 = "%box_float32"
external box_int32 : int32_u -> int32 = "%box_int32"
external box_int64 : int64_u -> int64 = "%box_int64"
external box_nativeint : nativeint_u -> nativeint = "%box_nativeint"
val eq_f64 : float# -> float# -> bool = <fun>
val eq_f32 : float32_u -> float32_u -> bool = <fun>
val eq_i32 : int32_u -> int32_u -> bool = <fun>
val eq_i64 : int64_u -> int64_u -> bool = <fun>
val eq_n : nativeint_u -> nativeint_u -> bool = <fun>
|}]

(* A string allocated at runtime so that the two blocks under comparison share
   a pointer rather than a statically allocated constant. *)
let s = String.make 3 'a'
[%%expect{|
val s : string = "aaa"
|}]

(* Values *)

type 'a vrec = { v : 'a }

type mixed = { mx : int64_u; my : string }
type with_product = { wp : #(int64_u * string); wq : int }
type all_flat = { af : float#; ag : int32_u }
type float_record = { fr1 : float; fr2 : float }
type variant = A of int64_u * string | B | C of #(float# * int)

(* Boxing a value gives a block shaped like [vrec] whose field is the value. *)
let check_value x =
  let boxed = box x in
  check_shape boxed { v = x };
  assert ((Obj.magic boxed : _ vrec).v == x)

let () =
  check_value 42;
  check_value s;
  check_value 3.25;
  check_value (Some s);
  check_value { mx = #1L; my = s };
  check_value { wp = #(#2L, s); wq = 3 };
  check_value { af = #1.5; ag = #4l };
  check_value { fr1 = 1.5; fr2 = 2.5 };
  check_value (A (#5L, s));
  check_value B;
  check_value (C #(#6.5, 7))
[%%expect{|
type 'a vrec = { v : 'a; }
type mixed = { mx : int64_u; my : string; }
type with_product = { wp : #(int64_u * string); wq : int; }
type all_flat = { af : float#; ag : int32_u; }
type float_record = { fr1 : float; fr2 : float; }
type variant = A of int64_u * string | B | C of #(float# * int)
val check_value : 'a -> unit = <fun>
|}]

(* Unboxed numbers stored in a whole word *)

(* for now, we box [float#] as if they were not addressable. *)
type f64rec = { f64 : float# }
type i64rec = { i64 : int64_u }
type nrec = { n : nativeint_u }
type irec = { i : int# }
[%%expect{|
type f64rec = { f64 : float#; }
type i64rec = { i64 : int64_u; }
type nrec = { n : nativeint_u; }
type irec = { i : int#; }
|}]

let () =
  check_shape (box #3.25) { f64 = #3.25 };
  check_shape (box #42L) { i64 = #42L };
  check_shape (box #42n) { n = #42n };
  check_shape (box (untag_int 42)) { i = untag_int 42 };
  assert (eq_f64 (Obj.magic (box #3.25) : f64rec).f64 #3.25);
  assert (eq_i64 (Obj.magic (box #42L) : i64rec).i64 #42L);
  assert (eq_n (Obj.magic (box #42n) : nrec).n #42n);
  assert (eq_int (Obj.magic (box (untag_int 42)) : irec).i (untag_int 42))
[%%expect{|
|}]

(* Unboxed numbers narrower than a word. for now, these are treated as
   addressable and boxed as tag-0 blocks, NOT as tagged immediates. eventually,
   addressable kinds will allow expression of both behaviors. *)

type f32rec = { f32 : float32_u }
type i32rec = { i32 : int32_u }
type i8rec = { i8 : int8# }
type i16rec = { i16 : int16# }
[%%expect{|
type f32rec = { f32 : float32_u; }
type i32rec = { i32 : int32_u; }
type i8rec = { i8 : int8#; }
type i16rec = { i16 : int16#; }
|}]

let () =
  check_shape (box #3.25s) { f32 = #3.25s };
  check_shape (box #42l) { i32 = #42l };
  check_shape (box #42s) { i8 = #42s };
  check_shape (box #42S) { i16 = #42S };
  assert (eq_f32 (Obj.magic (box #3.25s) : f32rec).f32 #3.25s);
  assert (eq_i32 (Obj.magic (box #42l) : i32rec).i32 #42l);
  assert (eq_i8 (Obj.magic (box #42s) : i8rec).i8 #42s);
  assert (eq_i16 (Obj.magic (box #42S) : i16rec).i16 #42S)
[%%expect{|
|}]

(* Unboxed products *)

type p_values = { a : int; b : string }
type p_flat_first = { c : int64_u; d : string }
type p_value_first = { e : string; f : int64_u }
type p_many = { g : int64_u; h : float#; k : string; l : int; m : int64_u }
type p_nested = { n1 : #(int64_u * string); n2 : float# }

let () =
  check_shape (box #(42, s)) { a = 42; b = s };
  let r : p_values = Obj.magic (box #(42, s)) in
  assert (r.a = 42 && r.b == s)
[%%expect{|
type p_values = { a : int; b : string; }
type p_flat_first = { c : int64_u; d : string; }
type p_value_first = { e : string; f : int64_u; }
type p_many = { g : int64_u; h : float#; k : string; l : int; m : int64_u; }
type p_nested = { n1 : #(int64_u * string); n2 : float#; }
|}]

(* CR zeisbach: add tests for mixed tuples after rebasing onto them *)

(*
external box_obj : ('a : any). 'a -> Obj.t = "%box" [@@layout_poly]

let () =
  check_shape (box_obj #(#42L, s)) { c = #42L; d = s };
  check_shape (box_obj #(s, #42L)) { e = s; f = #42L };
  check_shape (box_obj #(#1L, #2.5, s, 3, #4L))
    { g = #1L; h = #2.5; k = s; l = 3; m = #4L };
  check_shape (box_obj #(#(#42L, s), #2.5)) { n1 = #(#42L, s); n2 = #2.5 };
  check_shape (box_obj #(#(#42L, s), #2.5)) { q1 = #42L; q2 = s; q3 = #2.5 };
  let r : p_flat_first = Obj.obj (box_obj #(#42L, s)) in
  assert (eq_i64 r.c #42L && r.d == s);
  let r : p_many = Obj.obj (box_obj #(#1L, #2.5, s, 3, #4L)) in
  assert (eq_i64 r.g #1L && eq_f64 r.h #2.5 && r.k == s && r.l = 3
          && eq_i64 r.m #4L)
*)


type ur = { u1 : int64_u; u2 : string; u3 : int }

let () =
  let flat_first : p_flat_first = box #{ c = #42L; d = s } in
  check_shape flat_first { c = #42L; d = s };
  assert (eq_i64 flat_first.c #42L && flat_first.d == s);
  let value_first : p_value_first = box #{ e = s; f = #42L } in
  check_shape value_first { e = s; f = #42L };
  assert (value_first.e == s && eq_i64 value_first.f #42L);
  let many : p_many = box #{ g = #1L; h = #2.5; k = s; l = 3; m = #4L } in
  check_shape many { g = #1L; h = #2.5; k = s; l = 3; m = #4L };
  assert (eq_i64 many.g #1L && eq_f64 many.h #2.5 && many.k == s && many.l = 3
          && eq_i64 many.m #4L);
  let nested : p_nested = box #{ n1 = #(#42L, s); n2 = #2.5 } in
  check_shape nested { n1 = #(#42L, s); n2 = #2.5 };
  let #(n1a, n1b) = nested.n1 in
  assert (eq_i64 n1a #42L && n1b == s && eq_f64 nested.n2 #2.5);
  let boxed : ur = box #{ u1 = #42L; u2 = s; u3 = 7 } in
  check_shape boxed { u1 = #42L; u2 = s; u3 = 7 };
  assert (eq_i64 boxed.u1 #42L && boxed.u2 == s && boxed.u3 = 7)
[%%expect{|
type ur = { u1 : int64_u; u2 : string; u3 : int; }
|}]

(* All-void records. Natively the block is empty, which requires it to be
   statically allocated and hence immutable; in bytecode each void field is
   an empty block, so the record is not. Either way it must match the directly
   constructed record. *)

type all_void = { x : unit#; kept : unit# }

let () =
  let boxed : all_void = box #{ x = #(); kept = #() } in
  check_shape boxed { x = #(); kept = #() };
  assert ((not (native ())) || Obj.size (Obj.repr boxed) = 0)
[%%expect{|
type all_void = { x : unit#; kept : unit#; }
|}]

(* Local allocation. We have to globalize before passing to [Obj] helpers *)

external globalize : local_ 'a -> 'a = "%obj_dup"
[%%expect{|
external globalize : 'a @ local -> 'a = "%obj_dup"
|}]

let () =
  let local_ value = box s in
  check_shape (globalize value) { v = s };
  let local_ f = box #3.25 in
  check_shape (globalize f) { f64 = #3.25 };
  assert (eq_f64 (Obj.magic (globalize f) : f64rec).f64 #3.25);
  let local_ record : p_many = box #{ g = #1L; h = #2.5; k = s; l = 3; m = #4L } in
  check_shape (globalize record) { g = #1L; h = #2.5; k = s; l = 3; m = #4L };
  assert (eq_i64 record.g #1L && record.k == s && record.l = 3);
  let local_ inner = Some s in
  let local_ outer = box inner in
  assert ((Obj.magic (globalize outer) : string option vrec).v == inner)
[%%expect{|
|}]

(* Aliasing. This test would fail on bytecode if [box] did not deeply copy. *)

type inner = #{ ix : int; iy : int }
(* CR zeisbach: avoiding singleton unboxed record because it can be weird. \
   check what the correct/intended behavior is. *)
type outer = { mutable u : inner; tag : int }

let () =
  let un = #{ u = #{ ix = 1; iy = 2 }; tag = 7 } in
  let r : outer = box un in
  (* Write into the nested product in place. *)
  Stdlib_stable.Idx_mut.set r (.u.#ix) 10;
  assert (r.u.#ix = 10 && un.#u.#ix = 1);
  (* Replace the nested product wholesale. *)
  r.u <- #{ ix = 20; iy = 30 };
  assert (r.u.#iy = 30 && un.#u.#iy = 2)
[%%expect{|
type inner = #{ ix : int; iy : int; }
type outer = { mutable u : inner; tag : int; }
|}]

(* Mutability. If we said that the boxed version was [Immutable], optimizations
   could cause a stale value to be read. Simiarly, two different allocations
   could be CSE-ed away. *)
(* CR zeisbach: consider adding more of these tests, or adding a some tests that
   inspect the lambda directly? think about what to really test. *)
let () =
  let un = #{ u = #{ ix = Sys.opaque_identity 1; iy = 2 }; tag = 7 } in
  let r1 = (box un : outer @ global) in
  let r2 = (box un : outer @ global) in
  assert (r1 != r2);
  Stdlib_stable.Idx_mut.set r1 (.u.#ix) 40;
  assert (r1.u.#ix = 40 && r2.u.#ix = 1)
[%%expect{|
|}]

(* CR zeisbach: singleton (unboxed) records, pre-inherit *)
